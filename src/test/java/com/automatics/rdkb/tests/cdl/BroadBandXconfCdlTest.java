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

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

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

}
