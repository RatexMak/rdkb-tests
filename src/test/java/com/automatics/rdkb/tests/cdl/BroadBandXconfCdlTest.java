/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */

package com.automatics.rdkb.tests.cdl;

import java.util.Map;

import org.apache.commons.httpclient.HttpStatus;
import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.BroadBandRfcFeatureControlUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.utils.xconf.XConfUtils;

/**
 * Class for XCONF CDL tests 
 * @author anandam.s
 *
 */

public class BroadBandXconfCdlTest extends AutomaticsTestBase {

    /**
     * Trigger XCONF/DIFD HTTP CDL during router mode [ Deferred Reboot] and check Partner ID
     * <ol>
     * <li>PRE-CONDITION 1: To disable code big first</li>
     * <li>1.Set lanMode to router</li>
     * <li>2.Check Whether Device within Maintaince Window and if not ,Simulate the device to fall within maintaince
     * window by modifing the Maintaince window start and end time using RFC.</li>
     * <li>3.Configure XCONF server with required configuration using REST API</li>
     * <li>4.Reboot the device to initiate XCONF download.</li>
     * <li>5.verify the activation in progress has valid value.</li>
     * <li>6.verify the delay download log is logged in xconf.txt.0.</li>
     * <li>7.Verify XCONF server response</li>
     * <li>8.Verify whether download request is accepted</li>
     * <li>9.Verify code download successful</li>
     * <li>10.Verify whether device is rebooted after successful download in Maintenance window</li>
     * <li>11.Verify current image name from version.txt</li>
     * <li>12.Verify last reboot reason (only if the device is in Maintenance window)</li>
     * <li>13.Verify whether the Xconf query has Partner ID in 'xconf.txt.0' log file.</li>
     * <li>14.Verify whether the splunk query has the Partner ID.</li>
     * <li>POST-CONDITION 1: Remove tmp xconf files.</li>
     * <li>POST-CONDITION 2 : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION</li>
     * <li>POST-CONDITION 3 : Reset FirmwareUpgradeStartTime to default</li>
     * <li>POST-CONDITION 4 : Reset FirmwareUpgradeEndTime to default</li>
     * </ol>
     * 
     * @param device
     *            instance of {@link Dut}
     * 
     * @author styles mangalasseri, Vignesh
     * @refactor anandam
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-XCONF-1001", testDecription = "Verify XCONF/DIFD HTTP CDL during router mode [ Deferred Reboot] and check Partner ID")
    public void testToVerifyXconfDiffereredRebootHttpCdl(Dut device) {

	// execution status
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-XCONF-001";
	// step number
	int stepNumber = BroadBandTestConstants.CONSTANT_1;
	// variable to hold the server response
	// String response = null;
	String currentImageName = null;
	// image name to perform CDL
	String imageNameForCdl = null;
	// reboot immediately status
	String rebootImmediately = "false";
	// error message
	String errorMessage = null;
	// variable to hold the expected log message for validation
	String expectedLogMessage = null;
	// test step number
	String stepNum = "S" + stepNumber;
	String response = null;
	boolean isCdlDataPosted = false;
	boolean hasLatestBuildChanged = false;
	boolean logFileExists = false;
	String expectedPartnerId = null;
	String partnerId = null;
	long startTime = 0L;
	// Variable hold the device in maintenance window
	boolean isDeviceInMaintenanceWindow = false;
	// cdl log for validation
	String cdlLogsForValidation = null;
	String maintenanceWindowStartTime = null;
	String maintenanceWindowEndTime = null;
	//int NUMBER_OF_XCONF_OR_DCM_QUERY_PARAMETERS = 4;
	 String buildExtension = null;

	/** splunk start index **/
	//String SPLUNK_START_INDEX = "20m";

	/** Wait Time **/
	long waitTime = 15;
	/** Constant holds the maintenance window mapping **/
	Map<String, String> maintenanceWindow = null;

	try {
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE -TC-RDKB-XCONF-1001");
	    LOGGER.info(
		    "TEST DESCRIPTION: Trigger XCONF/DIFD HTTP CDL during router mode [ Deferred Reboot] and check Partner ID");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("PRE-CONDITION 1 : To disable code big first");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. set lanMode to router");
	    LOGGER.info(
		    "2. Check Whether Device within Maintaince Window  and Simulate the device to fall within maintaince window by modifing the Maintaince window start and end time using RFC.");
	    LOGGER.info("3.Configure XCONF server with required configuration using REST API");
	    LOGGER.info("4. Reboot the device to initiate XCONF download");
	    LOGGER.info("5 : verify the activation In progress has valid value");
	    LOGGER.info("6 : verify the delay download log is logged in xconf.txt.0");
	    LOGGER.info("7. Verify XCONF server response");
	    LOGGER.info("8. Verify whether download request is accepted");
	    LOGGER.info("9. Verify code download successful");
	    LOGGER.info("10. Verify whether device is rebooted after successful download in Maintainance Window");
	    LOGGER.info("11. Verify current image name from version.txt");
	    LOGGER.info("12. Verify last reboot reason (only if the device is in Maintenance window)");
	    LOGGER.info("13. Verify whether the Xconf query has Partner ID in 'xconf.txt.0' log file");
	  /*  LOGGER.info("14. Verify whether the splunk query has the Partner ID");*/
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION 1 : TO CLEAR THE CDL INFORMATION URL");
	    LOGGER.info("POST-CONDITION 2 : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION");
	    LOGGER.info("POST-CONDITION 3 : Reset FirmwareUpgradeStartTime to default");
	    LOGGER.info("POST-CONDITION 4 : Reset FirmwareUpgradeEndTime to default");
	    LOGGER.info("#######################################################################################");

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    /**
	     * STEP 1 : SET RDKB DEVICE LANMODE TO ROUTER
	     */
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Set rdkb device lanmode to router");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Execute webpa command : Device.X_CISCO_COM_DeviceControl.LanManagementEntry.1.LanMode with value as router");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should be able to set lanMode as router");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to set the lan mode to router";
	    try {
		status = BroadBandCommonUtils.setDeviceInRouterModeStatusUsingWebPaCommand(tapEnv, device);
	    } catch (Exception e) {
		errorMessage = "Exception occured while setting the lan mode to router " + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully verified the device lan mode as router");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    // Step 2
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Check Whether Device within Maintaince Window  and Simulate the device to fall within maintaince window by modifing the Maintaince window start and end time using RFC.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : 1.Copy and update /nvram/rfc.properties with mock RFC config server URL \r\n"
		    + "2.Post payload after replacing ESTB mac and enable/disable value to RFC server");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Successfully configured tthe Device MaintenanceWindow using RFC");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify decice acessibility";
	    maintenanceWindowStartTime = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME);
	    maintenanceWindowEndTime = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME);
	    try {
		isDeviceInMaintenanceWindow = FirmwareDownloadUtils.isDeviceInMaintenanceWindow(tapEnv, device);
	    } catch (TestException testException) {
		LOGGER.error(testException.getMessage());
	    }
	    // Setting the device to fall in maintenance window
	    try {
		maintenanceWindow = BroadBandCommonUtils
			.calculateFirmwareUpgradeMaintenanceWindowWithTimeInterval(tapEnv, device, waitTime);
		status = BroadBandCommonUtils.calculateAndSetMaintanenceWindow(device, tapEnv, waitTime,
			maintenanceWindow);
	    } catch (Exception e) {
		status = false;
		errorMessage += "Exception occured while setting the maintenance window ." + e.getMessage();
	    }
	    BroadBandCommonUtils.getMaintenanceWindowDetails(device, tapEnv);

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: Successfully configured tthe Device Maintenance window using RFC");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 3 : CONFIGURE XCONF SERVER WITH REQUIRED CONFIGURATION USING REST API
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : Configure XCONF server with required configuration using REST API with delaydownload 5 mins");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Create /nvram/swupdate.conf file and add the software update url.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Should be able to configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to configure XCONF server with required configuration using REST API";
	    try {
		currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		LOGGER.info("CURRENT FIRMWARE VERSION: " + currentImageName);
		//add signed extension to the image 
		buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
		try {
		    buildExtension = BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(device,
			    BroadBandTestConstants.PROP_KEY_SIGNED_EXTENSION);
		} catch (Exception e) {
		    buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
		    LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

		}
		
		imageNameForCdl = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
		LOGGER.info("LATEST FIRMWARE VERSION: " + imageNameForCdl);
		if (CommonMethods.isNull(imageNameForCdl)) {
		    LOGGER.info(" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		    imageNameForCdl=  BroadBandCommonUtils.getAutomaticsPropsValueByResolvingPlatform(device, BroadBandTestConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		    LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
		}

		if (CommonMethods.isNotNull(imageNameForCdl)) {
		    if (!imageNameForCdl.contains(currentImageName)) {
			BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, false,
				BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP, BroadBandTestConstants.CONSTANT_5);
			status = true;
			isCdlDataPosted = status;
		    } else {
			errorMessage = "Device current image and latest image to upgrade are same.";
		    }
		    
		} else {
		    errorMessage = "Unable to retrieve latest image name for RDKB devices";
		}
		tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CMD_GET_XCONF_TXT);
	    } catch (Exception e) {
		errorMessage = "Exception occured while configuring xconf server " + e.getMessage();
		LOGGER.error(errorMessage);
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: Successfully Configured XCONF server with required configuration.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 4 : INITIATE CDL USING WEBPA COMMAND
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Initiate cdl using webpa command");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE WEBPA COMMAND : Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow to true.");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Should be able to Initiate cdl using webpa command");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to initiate cdl using webpa command";
	    try {
		status = BroadBandXconfCdlUtils.initiateXconfCdlThroughWebpa(tapEnv, device);
	    } catch (Exception e) {
		errorMessage = "Exception occured while initiating cdl throgh webpa " + e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully initiated cdl using webpa command.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	   /**
	     * STEP 5 : Verify the activation In progress has valid value
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Failed to verify activation In Progress value";
	    status = false;

	    LOGGER.info("**********************************************************************************");

	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : verify the activation In progress has valid value");

	    LOGGER.info(
		    "STEP " + stepNumber + ": ACTION :  grep -I \"activationInProgress=\" /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : activationInProgress=false log should be present in xconf.log");
	    LOGGER.info("**********************************************************************************");
	    boolean activationValue = false;
	    try {
		logFileExists = CommonMethods.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_TMP_XCONFCDL);
		LOGGER.info(BroadBandCommandConstants.FILE_TMP_XCONFCDL + " Log file status " + logFileExists);
		response = tapEnv.executeWebPaCommand(device,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
		activationValue = CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.UNKNOWN_REBOOT_REASON) ? BroadBandTestConstants.BOOLEAN_VALUE_TRUE
				: BroadBandTestConstants.BOOLEAN_VALUE_FALSE;
		response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
			BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS,
			BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    } catch (Exception e) {
		LOGGER.error("Exception occured while executing command:" + e.getMessage());
	    }
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response,
			    BroadBandCommonUtils.concatStringUsingStringBuffer(
				    BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS,
				    String.valueOf(activationValue)));
	    if (!status && logFileExists) {
		response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS, String.valueOf(activationValue)),
			BroadBandCommandConstants.FILE_TMP_XCONFCDL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		LOGGER.info("Log in tmp file " + response);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.patternSearchFromTargetString(response,
				BroadBandCommonUtils.concatStringUsingStringBuffer(
					BroadBandTestConstants.STRING_ACTIVATION_INPROGRESS,
					String.valueOf(activationValue)));
	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully validated activationInProgress="
			+ activationValue + " in xconf");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	 /**
	     * STEP 6 : verify the delay download log is logged in xconf.txt.0
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    errorMessage = "Failed to verify the delay download log message";
	    status = false;
	    response = null;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP " + stepNumber + ": DESCRIPTION : verify the delay download log is logged in xconf.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : grep -I \"Device configured with download delay of 5 minutes\" /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Device configured with download delay of 5 minutes log should be present in xconf.txt.0");
	    LOGGER.info("**********************************************************************************");
	    try {
		response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
			BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
				.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5),
			BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH,
			BroadBandTestConstants.TWO_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    } catch (Exception e) {
		LOGGER.error("Exception occured while executing command:" + e.getMessage());
	    }
	    status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
		    BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
			    .replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5));
	    if (!status && logFileExists) {
		response = CommonUtils.searchLogFilesWithPollingInterval(tapEnv, device,
			BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
				.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5),
			BroadBandCommandConstants.FILE_TMP_XCONFCDL, BroadBandTestConstants.TWO_MINUTE_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		status = CommonMethods.isNotNull(response) && CommonUtils.patternSearchFromTargetString(response,
			BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
				.replace(BroadBandTestConstants.STRING_REPLACE, BroadBandTestConstants.STRING_5));

	    }

	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Successfully validated delay download text");
	    } else {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 7 : VERIFY XCONF FIRMWARE CONFIGURATION
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify XCONF firmware configuration");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE WEBPA COMMAND : Device.X_COMCAST-COM_Xcalibur.Client.xconfCheckNow to true.");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : /rdklogs/logs/xconf.txt.0 file should contains the configuration as configured");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to initiate cdl using webpa command";
	    try {

		    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTES);
		
		cdlLogsForValidation = BroadBandXconfCdlUtils.getCdlLogsForValidation(tapEnv, device);

		if (CommonMethods.isNotNull(cdlLogsForValidation)) {
		    BroadBandXconfCdlUtils.retrieveAndValidateXconfConfiguration(cdlLogsForValidation, imageNameForCdl,
			    rebootImmediately, BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP);
		    status = true;
		} else {
		    errorMessage = "Unable to read the contents from "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		}
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: Successfully validated that the XCONF configurations(download protocol, Current version and Upgrade version) are parsed successfully and logged in "
			+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	   /**
	     * STEP 8 : VERIFY WHETHER DOWNLOAD REQUEST IS ACCEPTED
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify whether download request is accepted.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : check the download accepted message in /rdklogs/logs/xconf.txt.0 .");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : /rdklogs/logs/xconf.txt.0 file should contains the accepted message in the format of \""
		    + BroadBandCdlConstants.ACCEPTED_MESSAGE_FOR_XCONF_CODE_DOWNLOAD + "\"");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify download request";
	    try {
		// format current image name which is retrieved using webpa command to compare with current image
		// version
		currentImageName = BroadBandCommonUtils
			.removeDifferentSignedExtensionsInRequestedBuildName(currentImageName);
		imageNameForCdl = BroadBandCommonUtils
			.removeDifferentSignedExtensionsInRequestedBuildName(imageNameForCdl);
		// verify the rejection message
		expectedLogMessage = BroadBandCdlConstants.ACCEPTED_MESSAGE_FOR_XCONF_CODE_DOWNLOAD
			.replaceAll(BroadBandCdlConstants.CONFIGURATION_CURRENT_IMAGE_NAME, currentImageName)
			.replaceAll(BroadBandCdlConstants.CONFIGURATION_REQUESTED_IMAGE_NAME, imageNameForCdl);
		// workaround to overcome the type error in RDK Code
		cdlLogsForValidation = cdlLogsForValidation.replaceAll("imgae", "image");
		status = cdlLogsForValidation.toLowerCase().contains(expectedLogMessage.toLowerCase());
		errorMessage = "Unable to find the XCONF " + BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP + " CDL accepted message '"
			+ expectedLogMessage + "' in " + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: Successfully validated the XCONF HTTP CDL accepted message in "
			+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 9 : VERIFY CODE DOWNLOAD IS SUCCESSFUL
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    // variable to block test script execution
	    boolean blockExecution = false;
	    expectedLogMessage = FirmwareDownloadUtils.getCdlDownloadSuccessLog(device);
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify code download is successful.");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : check the download successfull message in /rdklogs/logs/xconf.txt.0 .");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : " + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0
		    + " file should contains the code download success message in the format of \"" + expectedLogMessage
		    + "\"");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify code download successfull status";
	    try {
		if (CommonMethods.isNotNull(cdlLogsForValidation)) {
		    status = cdlLogsForValidation.toLowerCase().contains(expectedLogMessage.toLowerCase());
		    errorMessage = "Unable to find the XCONF HTTP CDL success message " + expectedLogMessage + " in "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		} else if (CommonMethods.isNotNull(cdlLogsForValidation) && cdlLogsForValidation.toLowerCase()
			.contains(BroadBandCdlConstants.XCONF_HTTP_DOWNLOAD_NOT_SUCCESSFUL.toLowerCase())) {
		    blockExecution = true;
		    errorMessage = "XCONF HTTP CDL not successful. obtained "
			    + BroadBandCdlConstants.XCONF_HTTP_DOWNLOAD_NOT_SUCCESSFUL + " in "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		} else {
		    errorMessage = "Unable to retrieve Code download completed message from "
			    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
		}
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: Successfully validated the XCONF HTTP CDL completed message in "
			+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, blockExecution);

	    // Step 10
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + ": DESCRIPTION : If Device Within Mainteince window, Check whether device getting rebooted within 1 min");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Check device is acessible");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device should not be accessible.");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify decice acessibility";
	    status = BroadBandCommonUtils.verifyDeviceRebootDuringMaintenanceWindow(device, tapEnv, maintenanceWindow);
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: Successfully verified that device went for reboot within maintenanceWindow");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    /**
	     * STEP 11 : VERIFY CURRENT IMAGE NAME FROM VERSION.TXT
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify current image name from version.txt");
	    LOGGER.info("STEP " + stepNumber + ": ACTION : Retrive Current image name from version.txt");
	    LOGGER.info("STEP " + stepNumber
		    + ": EXPECTED : Current image name from version.txt on both ARM or ATOM should be same.");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify current image name";
	    try {
		LOGGER.info("Waiting 1 minute");
		tapEnv.waitTill(BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
		FirmwareDownloadUtils.verifyCurrentImageNameBothInArmAndAtomConsole(tapEnv, device, imageNameForCdl);
		status = true;
		hasLatestBuildChanged = status;
	    } catch (Exception e) {
		status = false;
		errorMessage = e.getMessage();
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL: Successfully validated that the current image name from version.txt on both ARM or ATOM are as expected.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    /**
	     * STEP 12 : VERIFY CURRENT REBOOT REASON
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify current reboot reason");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : EXECUTE WEBPA COMMAND : Device.DeviceInfo.X_RDKCENTRAL-COM_LastRebootReason");
	    LOGGER.info(
		    "STEP " + stepNumber + ": EXPECTED : WebPA Parameter should return the value");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Failed to verify last reboot reason.";
	    status = FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
		    BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_DIFD_CDL_VIA_WEBPA_REBOOT)
		    || FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
			    BroadBandCdlConstants.EXPECTED_LAST_REBOOT_REASON_STATUS_AFTER_CDL)
		    || FirmwareDownloadUtils.verifyLastRebootReasonViaWebpa(tapEnv, device,
			    BroadBandTestConstants.REBOOT_REASON_RFC_REBOOT);
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully validated the reboot reason.");
	    } else {
		LOGGER.error("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	   /**
	     * STEP 13 : Verify whether the Xconf query has Partner ID in \"xconf.txt.0\" log file.
	     */
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber
		    + " : DESCRIPTION : Verify whether the Xconf query has Partner ID in \"xconf.txt.0\" log file");
	    LOGGER.info("STEP " + stepNumber
		    + " : ACTION : Execute command \"grep -i partnerId /rdklogs/logs/xconf.txt.0\" to verify partner ID in device console");
	    LOGGER.info("STEP " + stepNumber
		    + " : EXPECTED : XCONF query should have partnerId parameter in xconf.txt.0 log file");
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("***** RETRIEVING THE PARTNER ID USING WEBPA/DMCLI! *****");
	    errorMessage = "Unable to retrieve Partner ID using WebPA/DMCLI";
	    try {
		partnerId = BroadBandWebPaUtils.getParameterValuesUsingWebPaOrDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
		if (CommonMethods.isNotNull(partnerId)) {
		    expectedPartnerId = BroadBandCommonUtils.concatStringUsingStringBuffer("partnerId",
			    AutomaticsConstants.DELIMITER_EQUALS, partnerId);
		    LOGGER.info("***** EXEPECTED PARTNER ID STRING : " + expectedPartnerId);
		    LOGGER.info("***** RETRIEVING THE PARTNER ID FROM /RDKLOGS/LOGS/XCONF.TXT.0! *****");
		    errorMessage = "Unable to retrieve Partner ID from /rdklogs/logs/xconf.txt.0 !";
		    startTime = System.currentTimeMillis();
		    do {
			status = (cdlLogsForValidation.toLowerCase()).contains(expectedPartnerId.toLowerCase());
		    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
			    && !status && BroadBandCommonUtils.hasWaitForDuration(tapEnv,
				    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
		}
	    } catch (Exception e) {
		LOGGER.info("Exception occurred while validating partner ID in Xconf.txt File : " + e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP " + stepNumber
			+ ": ACTUAL : Firmware download is triggered Successfully and XCONF query is also having expected Partner ID in xconf.txt.0");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	 /*  *//**
	     * STEP 14 : Verify whether the splunk query has the Partner ID
	     *//*
	    stepNumber++;
	    stepNum = "S" + stepNumber;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Verify whether the splunk query has the Partner ID");
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTION : Retrieve the logs corresponding to MAC Address of gateway from Splunk");
	    LOGGER.info("STEP " + stepNumber + ": EXPECTED : Splunk response should have Partner ID  parameter");
	    LOGGER.info("**********************************************************************************");
	    startTime = System.currentTimeMillis();
	    do {
		try {
		    LOGGER.info(
			    "***** RETRIEVING THE LOGS FROM SPLUNK CORRESPONDING TO THE GATEWAY MAC ADDRESS! *****");
		    errorMessage = "Unable to retrieve logs correspoding to the gateway from Splunk";
		    String splunkSearchString = "index=rdk-json(" + settop.getHostMacAddress() + ")";
		    Collection<String> splunkResponse = SplunkUtils.searchInSplunk(tapEnv, splunkSearchString, null, -1,
			    SPLUNK_START_INDEX);
		    if (!splunkResponse.isEmpty() && splunkResponse.size() != BroadBandTestConstants.CONSTANT_0) {
			LOGGER.info("***** VERIFYING THE PARTNER ID IN LOGS RETRIEVED FROM SPLUNK! *****");
			errorMessage = "Splunk logs doesn't contain Partner ID";

			expectedPartnerId = BroadBandCommonUtils.concatStringUsingStringBuffer(
				BroadBandTestConstants.DOUBLE_QUOTE, "PartnerId", BroadBandTestConstants.DOUBLE_QUOTE,
				BroadBandTestConstants.DELIMITER_COLON, BroadBandTestConstants.DOUBLE_QUOTE, partnerId,
				BroadBandTestConstants.DOUBLE_QUOTE);
			LOGGER.info("***** EXEPECTED PARTNER ID STRING : " + expectedPartnerId);
			status = SplunkUtils.verifySplunkResults(splunkResponse, expectedPartnerId);

			if (!status) {
			    LOGGER.info("***** SPLUNK LOGS DOESN'T CONTAIN PARTNER ID, HENCE TRYING AGAIN! *****");
			}
		    } else {
			LOGGER.info("***** UNABLE TO RETRIEVE LOGS FROM SPLUNK, HENCE TRYING AGAIN! *****");
		    }
		} catch (Exception e) {
		    LOGGER.error("Exception at verifying splunk results : " + e.getMessage());
		}
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWENTY_MINUTES_IN_MILLIS
		    && !status
		    && BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.TEN_SECOND_IN_MILLIS));
	    if (status) {
		LOGGER.info("STEP " + stepNumber + ": ACTUAL : Splunk response is having the expected Partner ID");
	    } else {
		LOGGER.error("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);*/
	} catch (Exception exception) {
	    errorMessage = "Exception occurred during execution : " + exception.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    int postCondNumber = BroadBandTestConstants.CONSTANT_0;
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    tapEnv.executeCommandUsingSsh(device, "rm -rf /rdklogs/logs/xconf.txt");
	    if (logFileExists) {
		postCondNumber++;
		LOGGER.info("####################################################################");
		LOGGER.info("POST-CONDITION " + postCondNumber + " : DESCRIPTION : Remove tmp xconf files");
		LOGGER.info("POST-CONDITION " + postCondNumber + " : ACTION : Remove "
			+ BroadBandCommandConstants.FILE_TMP_XCONFCDL);
		LOGGER.info("POST-CONDITION " + postCondNumber + " : EXPECTED : Files removed successfully");
		LOGGER.info("####################################################################");
		status = false;

		errorMessage = "Failed to remove /tmp/Consolelog.txt file";
		status = BroadBandCommonUtils.removeFileandVerifyFileRemoval(tapEnv, device,
			BroadBandCommandConstants.FILE_TMP_XCONFCDL);
		if (status) {
		    LOGGER.info("POST-CONDITION " + postCondNumber + " : ACTUAL : File removed successfully");
		} else {
		    LOGGER.error("POST-CONDITION " + postCondNumber + " : ACTUAL : " + errorMessage);
		}
	    }
	    /**
	     * POST CONDITION : POST-CONDITION METHOD TO CLEAR THE CDL INFORMATION URL
	     */
	    if (isCdlDataPosted) {
		postCondNumber++;
		BroadBandPostConditionUtils.executePostConditionToClearCdlInfoInXconf(device, tapEnv, postCondNumber);
	    }
	    /**
	     * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION.
	     */
	    LOGGER.info("BUILD TO REVERT :" +currentImageName);
	    BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, hasLatestBuildChanged, false,
		    postCondNumber, currentImageName.concat(buildExtension));
	    postCondNumber++;
	    status = false;
	    LOGGER.info("####################################################################");
	    LOGGER.info(
		    "POST-CONDITION " + postCondNumber + ": DESCRIPTION : Reset FirmwareUpgradeStartTime to default");
	    LOGGER.info("POST-CONDITION " + postCondNumber
		    + ": ACTION : Execute command: dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeStartTime with initial value or Default");
	    LOGGER.info("POST-CONDITION " + postCondNumber + ": EXPECTED : Value set successfully");
	    LOGGER.info("####################################################################");
	    if (CommonMethods.isNotNull(maintenanceWindowStartTime)) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
			BroadBandTestConstants.CONSTANT_0, maintenanceWindowStartTime);
	    } else {
		/*status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME,
			BroadBandTestConstants.CONSTANT_0,
			BroadBandTestConstants.DEFAULT_FIRMWARE_UPGRADE_MAINTENANCE_WINDOW_START_TIME);*/
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION " + postCondNumber + ": ACTUAL : "
			+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME
			+ "value updated Successfully");
	    } else {
		LOGGER.error("POST-CONDITION " + postCondNumber + ": ACTUAL : Failed to update  "
			+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_START_TIME + "");
	    }

	    postCondNumber++;
	    status = false;
	    LOGGER.info("####################################################################");
	    LOGGER.info("POST-CONDITION " + postCondNumber + ": DESCRIPTION : Reset FirmwareUpgradeEndTime to default");
	    LOGGER.info("POST-CONDITION " + postCondNumber
		    + ": ACTION : Execute command:dmcli eRT setv Device.DeviceInfo.X_RDKCENTRAL-COM_MaintenanceWindow.FirmwareUpgradeEndTime intitial value or Default");
	    LOGGER.info("POST-CONDITION " + postCondNumber + ": EXPECTED : Value set successfully");
	    LOGGER.info("####################################################################");
	    if (CommonMethods.isNotNull(maintenanceWindowEndTime)) {
		status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME,
			BroadBandTestConstants.CONSTANT_0, maintenanceWindowEndTime);
	    } else {
		/*status = BroadBandWebPaUtils.setAndVerifyParameterValuesUsingWebPaorDmcli(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME,
			BroadBandTestConstants.CONSTANT_0,
			BroadBandTestConstants.DEFAULT_FIRMWARE_UPGRADE_MAINTENANCE_WINDOW_END_TIME);*/
	    }
	    if (status) {
		LOGGER.info("POST-CONDITION " + postCondNumber + ": ACTUAL : "
			+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME
			+ " Value set successfully");
	    } else {
		LOGGER.error("POST-CONDITION " + postCondNumber + ": ACTUAL : Failed to update "
			+ BroadBandWebPaConstants.WEBPA_COMMAND_MAINTENANCE_WINDOW_END_TIME + "");
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-1001");
	LOGGER.info("#######################################################################################");
    }
    /**
     * <li>1. Update auto exclude enable as false using webpa<\li>
     * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
     * <li>3. Verify prod xconf url used as default for cdl in xconf log file<\li>
     * <li>4. Verify http 404 response from the xconf log file<\li>
     * 
     * @author ArunKumar Jayachandran
     * @refactor anandam
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1001")
    public void verifyAutoExcludeFromFWUpgrade1(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is empty & swupdate.conf is empty");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as false using webpa");
	LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
	LOGGER.info("3. Verify prod xconf url used as default for cdl in xconf log file");
	LOGGER.info("4. Verify http 404 response from the xconf log file");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-101";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store response
	String response = null;
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	long startTime = BroadBandTestConstants.CONSTANT_0;
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }
	    /* Step 1: configure auto exclude enable as false */
	    stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
		    BroadBandTestConstants.FALSE);
	    /* Step 2: configure auto exclude url as empty */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
		    BroadBandTestConstants.EMPTY_STRING, true);

	    // step 3:
	    stepCount++;
	    stepNumber = "s" + stepCount;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION: Verify prod xconf url used as default for cdl in xconf log file");
	    LOGGER.info("STEP " + stepCount
		    + ": ACTION: Execute command: grep -i \"Device retrieves firmware update from url=\" /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepCount
		    + ": EXPECTED: Should get the log message for xconf url as " + BroadbandPropertyFileHandler.getProdCDLServerURL());
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message firmware update url as " + BroadbandPropertyFileHandler.getProdCDLServerURL() +"  in xconf log file";
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.FILE_FIRMWARE_SCHED_SH);
	    startTime = System.currentTimeMillis();
	    do {
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
			BroadbandPropertyFileHandler.getProdCDLServerURL());
		response = CommonMethods.isNotNull(response) ? response.trim() : null;
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.TWO_MINUTE_IN_MILLIS
		    && CommonMethods.isNull(response));
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTraceConstants.LOG_MESSAGE_FW_UPDATE_FROM_URL);
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified prod xconf url used for CDL in xconf log file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepCount++;
	    stepNumber = "s" + stepCount;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Verify http 404 response from the xconf log file");
	    LOGGER.info("STEP " + stepCount
		    + ": ACTION: Execute command: grep -i \"HTTP RESPONSE CODE is 404\" /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepCount
		    + ": EXPECTED: Should get the HTTP code as 404 and CDL should skip for prod xconf url");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message http 404 in xconf log file";
	    status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFiles(tapEnv, device,
		    BroadBandTraceConstants.LOG_MESSAGE_HTTP_RESPONSE_CODE_404,
		    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
	    if (status) {
		LOGGER.info("STEP " + stepCount
			+ ": ACTUAL: Successfully verified the log message http 404 response in xconf log file");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1001");
	// ###############################################################//
    }

    /**
     * <li>1. Update auto exclude enable as false using webpa<\li>
     * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
     * <li>3. Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware
     * version through xconf<\li>
     * 
     * @author ArunKumar Jayachandran
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1002")
    public void verifyAutoExcludeFromFWUpgrade2(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1002");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is empty & swupdate.conf is MOCK XCONF URL");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as false using webpa");
	LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
	LOGGER.info(
		"3. Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-102";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	String initialFirmwareVersion = null;
	String cdlLogsForValidation = null;
	String latestFirmwareVersion = null;
	boolean hasLatestBuildChanged = false;
	String buildExtension = null;
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }
	    /* Step 1: configure auto exclude enable as false */
	    stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
		    BroadBandTestConstants.FALSE);
	    /* Step 2: configure auto exclude url as empty */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
		    BroadBandTestConstants.EMPTY_STRING, true);

	    // Step 3:
	    stepNumber = "s" + ++stepCount;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION : Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
	    initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
	    // add signed extension to the current image
	    buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
	    try {
		buildExtension = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PROP_KEY_SIGNED_EXTENSION);
	    } catch (Exception e) {
		buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
		LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

	    }
	    latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    if (CommonMethods.isNull(latestFirmwareVersion)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
	    }
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
		    latestFirmwareVersion, true);
	    errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
	    status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
		    tapEnv, latestFirmwareVersion, cdlLogsForValidation);
	    LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
	    if (status) {
		hasLatestBuildChanged = status;
		LOGGER.info(
			"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
	    } else {
		LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (hasLatestBuildChanged) {
		/**
		 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION.
		 */
		BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
			BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1002");
	// ###############################################################//
    }

    /**
     * <li>1. Update auto exclude enable as false using webpa<\li>
     * <li>2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf file<\li>
     * <li>3. Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware
     * version through xconf<\li>
     * 
     * @author ArunKumar Jayachandran
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1003")
    public void verifyAutoExcludeFromFWUpgrade3(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is CI XCONF URL & swupdate.conf is MOCK XCONF URL");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as false using webpa");
	LOGGER.info("2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf file");
	LOGGER.info(
		"3. Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-103";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	String initialFirmwareVersion = null;
	String cdlLogsForValidation = null;
	String latestFirmwareVersion = null;
	boolean hasLatestBuildChanged = false;
	String ciXconfUrl = BroadbandPropertyFileHandler.getCIServerSwupdateURL();
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }

	    /* Step 1: configure auto exclude enable as false */
	    stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
		    BroadBandTestConstants.FALSE);
	    /* Step 2: configure auto exclude url as empty */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, ciXconfUrl, true);

	    // Step 3:
	    stepNumber = "s" + ++stepCount;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION : Update swupdate.conf file with MOCK xcocnf url and verify triggered cdl successfull with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
	    initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
	    latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    if (CommonMethods.isNull(latestFirmwareVersion)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
	    }
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
		    latestFirmwareVersion, true);
	    errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
	    status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
		    tapEnv, latestFirmwareVersion, cdlLogsForValidation);
	    LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
	    if (status) {
		hasLatestBuildChanged = status;
		LOGGER.info(
			"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
	    } else {
		LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (hasLatestBuildChanged) {
		/**
		 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION.
		 */
		BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
			BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1003");
	// ###############################################################//
    }

    /**
     * <li>1. Update auto exclude enable as false using webpa<\li>
     * <li>2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf file<\li>
     * <li>3. Verify triggered cdl successfull with latest firmware version through xconf without swupdate.conf
     * file<\li>
     * 
     * @author ArunKumar Jayachandran
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1004")
    public void verifyAutoExcludeFromFWUpgrade4(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1004");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is false url is MOCK XCONF URL & swupdate.conf is empty");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as false using webpa");
	LOGGER.info("2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf file");
	LOGGER.info(
		"3. Verify triggered cdl successfull with latest firmware version through xconf without swupdate.conf file");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-104";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// variable declaration ends
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	String mockXconfUrl = XConfUtils.getXconfServerUrl(device);
	String initialFirmwareVersion = null;
	String cdlLogsForValidation = null;
	String latestFirmwareVersion = null;
	boolean hasLatestBuildChanged = false;
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }
	    /* Step 1: configure auto exclude enable as false */
	    stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
		    BroadBandTestConstants.FALSE);
	    /* Step 2: configure auto exclude url as empty */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, mockXconfUrl, true);

	    // Step 3:
	    stepNumber = "s" + ++stepCount;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION : Verify triggered cdl successfull with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
	    initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
	    latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    if (CommonMethods.isNull(latestFirmwareVersion)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
	    }
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
		    latestFirmwareVersion, false);
	    errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
	    status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
		    tapEnv, latestFirmwareVersion, cdlLogsForValidation);
	    LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
	    if (status) {
		hasLatestBuildChanged = status;
		LOGGER.info(
			"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
	    } else {
		LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (hasLatestBuildChanged) {
		/**
		 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION.
		 */
		BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
			BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
	    }
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1004");
	// ###############################################################//
    }

    /**
     * <li>1. Update auto exclude enable as true using RFC<\li>
     * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
     * <li>3. Verify device is excluded from FW upgrade for triggered CDL<\li>
     * <li>4. verify xconf file is empty<\li>
     * 
     * @author ajayac200
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1005")
    public void verifyAutoExcludeFromFWUpgrade5(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1005");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is empty & swupdate.conf is empty");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as true using RFC");
	LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
	LOGGER.info("3. Verify device is excluded from FW upgrade for triggered CDL");
	LOGGER.info("4. verify xconf file is empty");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-105";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store response
	String response = null;
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }
	    /* Step 1: configure auto exclude enable as true */
	    stepNumber = "s" + stepCount;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Update auto exclude enable as true using RFC");
	    LOGGER.info("STEP " + stepCount
		    + ": ACTION: Execute command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as true");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: Should update the AutoExclude.Enable parameter as true");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update the AutoExclude.Enable parameter as true using RFC";
	    status = BroadBandRfcFeatureControlUtils.enableOrDisableFeatureByRFC(tapEnv, device,
		    BroadBandTestConstants.FEATURE_NAME_AUTO_EXCLUDE, true);
	    if (status) {
		LOGGER.info(
			"STEP " + stepCount + ": ACTUAL: Successfully updated AutoExclude.Enable as true using RFC");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    /* Step 2: configure auto exclude url as empty */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
		    BroadBandTestConstants.EMPTY_STRING, true);

	    // step 3:
	    stepNumber = "s" + ++stepCount;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION : Verify device is excluded from FW upgrade for triggered CDL");
	    LOGGER.info("STEP " + stepCount
		    + ": ACTION : Trigger the cdl and verify the excluded log message in Console log");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED : Device should log excluded from FW upgrade message");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.FILE_FIRMWARE_SCHED_SH);
	    long startTime = System.currentTimeMillis();
	    do {
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
			BroadBandTraceConstants.LOG_MESSAGE_EXCLUDED_FROM_FW_UPGRADE);
		response = CommonMethods.isNotNull(response) ? response.trim() : null;
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && CommonMethods.isNull(response));
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info("STEP " + stepCount + " : ACTUAL : Cdl not initiated as auto xconf enable is false");
	    } else {
		LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    // step 4:
	    stepCount++;
	    stepNumber = "s" + stepCount;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: verify xconf file is empty");
	    LOGGER.info("STEP " + stepCount + ": ACTION: Execute command: cat /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: xconf log file should be empty");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Firmware download started";
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    status = CommonMethods.isNull(response)
		    || !CommonMethods.isFileExists(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified xconf log file is empty");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION: DESCRIPTION: Update auto exclude enable as false using webpa");
	    LOGGER.info(
		    "POST-CONDITION: ACTION: Execute webpa command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as true");
	    LOGGER.info(
		    "POST-CONDITION: EXPECTED: Webpa set operation should success and auto excluded should be enabled");
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = (HttpStatus.SC_OK == CommonMethods.clearParamsInServer(device, tapEnv, false,
		    BroadBandTestConstants.FEATURE_NAME_AUTO_EXCLUDE));
	    LOGGER.info("POST-CONDITION: ACTUAL: Disabled firmware exclusion status as false");
	    LOGGER.info("POST-CONFIGURATIONS: FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1005");
	// ###############################################################//
    }

    /**
     * <li>1. Update auto exclude enable as true using webpa<\li>
     * <li>2. Update Xconf url as empty using dmcli & delete swupdate.conf file<\li>
     * <li>3. Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version
     * through xconf<\li>
     * 
     * @author ajayac200
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1006")
    public void verifyAutoExcludeFromFWUpgrade6(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1006");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is empty & swupdate.conf is MOCK XCONF URL");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as true using webpa");
	LOGGER.info("2. Update Xconf url as empty using dmcli & delete swupdate.conf file");
	LOGGER.info(
		"3. Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-106";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	String cdlLogsForValidation = null;
	String initialFirmwareVersion = null;
	String latestFirmwareVersion = null;
	boolean hasLatestBuildChanged = false;
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }
	    LOGGER.info("#######################################################################################");

	    /* Step 1: configure auto exclude enable as true */
	    stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
		    BroadBandTestConstants.TRUE);

	    /* Step 2: configure auto exclude url as empty */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount,
		    BroadBandTestConstants.EMPTY_STRING, true);

	    stepNumber = "s" + ++stepCount;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION : Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
	    initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
	    latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    if (CommonMethods.isNull(latestFirmwareVersion)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
	    }
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
		    latestFirmwareVersion, true);
	    errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
	    status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
		    tapEnv, latestFirmwareVersion, cdlLogsForValidation);
	    LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
	    if (status) {
		hasLatestBuildChanged = status;
		LOGGER.info(
			"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
	    } else {
		LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    if (hasLatestBuildChanged) {
		/**
		 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION.
		 */
		BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
			BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
	    }
	    BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1006");
	// ###############################################################//
    }

    /**
     * <li>1. Update auto exclude enable as true using webpa<\li>
     * <li>2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf file<\li>
     * <li>3. Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version
     * through xconf<\li>
     * 
     * @author ajayac200
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1007")
    public void verifyAutoExcludeFromFWUpgrade7(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1007");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is CI XCONF URL & swupdate.conf is MOCK XCONF URL");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as true using webpa");
	LOGGER.info("2. Update Xconf url as CI XCONF URL using webpa & delete swupdate.conf file");
	LOGGER.info(
		"3. Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-107";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	String cdlLogsForValidation = null;
	String initialFirmwareVersion = null;
	String latestFirmwareVersion = null;
	boolean hasLatestBuildChanged = false;
	String ciXconfUrl = BroadbandPropertyFileHandler.getCIServerSwupdateURL();
	String buildExtension = null;
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }
	    /* Step 1: configure auto exclude enable as false */
	    stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
		    BroadBandTestConstants.TRUE);

	    /* Step 2: configure auto exclude url as empty */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, ciXconfUrl, true);

	    stepNumber = "s" + ++stepCount;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION : Update swupdate.conf file with MOCK url & Verify triggered cdl successfull with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": ACTION : Trigger the cdl with latest firmware version through xconf");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED : Cdl must successful with latest firmware version");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
	    initialFirmwareVersion = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
	    LOGGER.info("CURRENT FIRMWARE VERSION: " + initialFirmwareVersion);
	    // add signed extension to the current image
	    buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
	    try {
		buildExtension = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PROP_KEY_SIGNED_EXTENSION);
	    } catch (Exception e) {
		buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
		LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

	    }
	    latestFirmwareVersion = tapEnv.getLatestBuildImageVersionForCdlTrigger(device, false);
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    if (CommonMethods.isNull(latestFirmwareVersion)) {
		LOGGER.info(
			" GA image obtained from deployed version service is null. Hence getting the image from property file ");
		latestFirmwareVersion = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		LOGGER.info("Latest Firmware version from property file: " + latestFirmwareVersion);
	    }
	    LOGGER.info("LATEST FIRMWARE VERSION: " + latestFirmwareVersion);
	    cdlLogsForValidation = BroadBandXconfCdlUtils.triggerXconfCodeDownloadWithSwupdateOption(device, tapEnv,
		    latestFirmwareVersion, true);
	    errorMessage = "Failed to get cdl logs from /rdklogs/logs/xconf.txt.0";
	    status = CommonMethods.isNotNull(cdlLogsForValidation) && BroadBandXconfCdlUtils.verifyCdlandReboot(device,
		    tapEnv, latestFirmwareVersion, cdlLogsForValidation);
	    LOGGER.info("FLASHED THE LATEST BUILD ON THE DEVICE: " + status);
	    if (status) {
		hasLatestBuildChanged = status;
		LOGGER.info(
			"STEP " + stepCount + " : ACTUAL : Cdl successful with latest firmware version through xconf");
	    } else {
		LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    if (hasLatestBuildChanged) {
		/**
		 * POST CONDITION : VERIFY THAT DEVICE IS UPGRADED WITH THE PREVIOUS FIRMWARE VERSION.
		 */
		BroadBandPostConditionUtils.executePostConditionToTriggerCdl(device, tapEnv, true, false,
			BroadBandTestConstants.CONSTANT_1, initialFirmwareVersion);
	    }
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
	    CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1007");
	// ###############################################################//
    }

    /**
     * <li>1. Update auto exclude enable as true using webpa<\li>
     * <li>2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf file<\li>
     * <li>3. Verify device is excluded from FW upgrade for triggered CDL<\li>
     * <li>4. verify xconf file is empty<\li>
     * 
     * @author ajayac200
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-CDL-EXCLUDE-1008")
    public void verifyAutoExcludeFromFWUpgrade8(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-CDL-EXCLUDE-1008");
	LOGGER.info(
		"TEST DESCRIPTION: Test to verify fw update exclusion & url selection as exclusion is true url is MOCK XCONF URL & swupdate.conf is empty");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Update auto exclude enable as true using webpa");
	LOGGER.info("2. Update Xconf url as MOCK XCONF URL using webpa & delete swupdate.conf file");
	LOGGER.info("3. Verify device is excluded from FW upgrade for triggered CDL");
	LOGGER.info("4. verify xconf file is empty");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-CDL-EXCLUDE-108";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	String response = null;
	int stepCount = BroadBandTestConstants.CONSTANT_1;
	String mockXconfUrl = XConfUtils.getXconfServerUrl(device);
	// variable declaration ends

	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    /**
	     * PRE-CONDITION 1 : Pre-Condition method to disable code big first.
	     */
	    BroadBandPreConditionUtils.executePreConditionToDisableCodeBigFirst(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);
	    LOGGER.info("#######################################################################################");

	    try {
		tapEnv.executeCommandUsingSsh(device,
			BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CMD_ECHO,
				BroadBandTestConstants.TEXT_DOUBLE_QUOTE, BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				BroadBandTestConstants.STRING_GREATER_SYMBOL,
				BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
				BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0));
	    } catch (Exception e) {
		LOGGER.info("Failed to clear xconf log file " + e.getMessage());
	    }
	    /* Step 1: configure auto exclude enable as true */
	    stepCount = configureAutoExcludeEnableParameter(device, tapEnv, testCaseId, stepCount,
		    BroadBandTestConstants.TRUE);

	    /* Step 2: configure auto exclude url as mockXconfUrl */
	    stepCount = configureAutoExcludeUrlParameter(device, tapEnv, testCaseId, ++stepCount, mockXconfUrl, true);

	    // step 3:
	    stepNumber = "s" + ++stepCount;
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP " + stepCount
		    + ": DESCRIPTION : Verify device is excluded from FW upgrade for triggered CDL");
	    LOGGER.info("STEP " + stepCount
		    + ": ACTION : Trigger the cdl and verify the excluded log message in Console log");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED : Device should log excluded from FW upgrade message");
	    LOGGER.info("**********************************************************************************");
	    errorMessage = "Unable to trigger the image upgrade on the device with latest firmware version.";
	    tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.FILE_FIRMWARE_SCHED_SH);
	    long startTime = System.currentTimeMillis();
	    do {
		tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
		response = BroadBandCommonUtils.searchLogsInArmConsoleOrConsole(device, tapEnv,
			BroadBandTraceConstants.LOG_MESSAGE_EXCLUDED_FROM_FW_UPGRADE);
		response = CommonMethods.isNotNull(response) ? response.trim() : null;
	    } while ((System.currentTimeMillis() - startTime) < BroadBandTestConstants.THREE_MINUTE_IN_MILLIS
		    && CommonMethods.isNull(response));
	    status = CommonMethods.isNotNull(response);
	    if (status) {
		LOGGER.info("STEP " + stepCount + " : ACTUAL : Cdl not initiated as auto xconf enable is false");
	    } else {
		LOGGER.error("STEP " + stepCount + " : ACTUAL :" + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);

	    // step 4:
	    stepCount++;
	    stepNumber = "s" + stepCount;
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP " + stepCount + ": DESCRIPTION: verify xconf file is empty");
	    LOGGER.info("STEP " + stepCount + ": ACTION: Execute command: cat /rdklogs/logs/xconf.txt.0");
	    LOGGER.info("STEP " + stepCount + ": EXPECTED: xconf log file should be empty");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Firmware download started";
	    response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandTestConstants.CAT_COMMAND + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    status = CommonMethods.isNull(response)
		    || !CommonMethods.isFileExists(device, tapEnv, BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (status) {
		LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified xconf log file is empty");
	    } else {
		LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying auto exclude from firmware upgrade" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-CDL-EXCLUDE-1008");
	// ###############################################################//
    }

    public static int configureAutoExcludeEnableParameter(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
	    int stepCount, String flag) {
	String stepNumber = null;
	boolean status = false;
	String errorMessage = null;

	stepNumber = "s" + stepCount;
	status = false;
	LOGGER.info("******************************************************************************");
	LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Update auto exclude enable as " + flag + " using webpa");
	LOGGER.info("STEP " + stepCount
		+ ": ACTION: Execute webpa command:Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as "
		+ flag);
	LOGGER.info("STEP " + stepCount + ": EXPECTED: Webpa set operation should success and auto excluded should be "
		+ flag);
	LOGGER.info("******************************************************************************");
	errorMessage = "Failed to update the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.Enable as "
		+ flag + " using webpa";
	status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLUDED_ENABLE, BroadBandTestConstants.CONSTANT_3,
		flag, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	if (status) {
	    LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully verified auto excluded enable as " + flag);
	} else {
	    LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	}
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	// ##################################################################################################//
	return stepCount;
    }

    public static int configureAutoExcludeUrlParameter(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
	    int stepCount, String xconfUrl, boolean isFileRemovalRequired) {
	String stepNumber = null;
	boolean status = false;
	String errorMessage = null;

	stepNumber = "s" + stepCount;
	status = false;
	LOGGER.info("******************************************************************************");
	LOGGER.info("STEP " + stepCount + ": DESCRIPTION: Update Xconf url as " + xconfUrl
		+ " using webpa or dmcli & delete swupdate.conf file");
	LOGGER.info("STEP " + stepCount
		+ ": ACTION: Execute webpa or dmcli command: Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.XconfUrl string "
		+ xconfUrl + " & rm -rf /nvram/swupdate.conf");
	LOGGER.info("STEP " + stepCount + ": EXPECTED: Xconf url should be " + xconfUrl
		+ " and swupdate.conf file should be removed successfully");
	LOGGER.info("******************************************************************************");
	errorMessage = "Failed to update the parameter Device.DeviceInfo.X_RDKCENTRAL-COM_RFC.Feature.FWUpdate.AutoExcluded.XconfUrl as "
		+ xconfUrl + " or failed to delete the swupdate.conf file";
	if (xconfUrl.equalsIgnoreCase(BroadBandTestConstants.EMPTY_STRING)) {
	    status = DmcliUtils.verifyElseSetParameterUsingDmcli(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLCUDED_XCONF_URL,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_STRING_PARAMETER, xconfUrl);
	} else {
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FWUPDATE_AUTO_EXCLCUDED_XCONF_URL,
		    BroadBandTestConstants.CONSTANT_0, xconfUrl, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	}

	if (isFileRemovalRequired && status) {
	    status = false;
	    status = CommonUtils.deleteFile(device, tapEnv, BroadBandTestConstants.SOFTWARE_UPDATE_CONF_FILE);
	}
	if (status) {
	    LOGGER.info("STEP " + stepCount + ": ACTUAL: Successfully updated the auto exclude xconf url as " + xconfUrl
		    + " and removed swupdate.conf file");
	} else {
	    LOGGER.error("STEP " + stepCount + ": ACTUAL: " + errorMessage);
	}
	tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	// ##################################################################################################//
	return stepCount;
    }

}
