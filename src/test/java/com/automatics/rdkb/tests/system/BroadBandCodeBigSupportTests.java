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
package com.automatics.rdkb.tests.system;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCdlConstants;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandPropertyKeyConstants;
import com.automatics.rdkb.constants.BroadBandTelemetryConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.cdl.CodeDownloadUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandCodeBigSupportTests extends AutomaticsTestBase {

    /**
     * Method to verify log message for communication type
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            Dut
     * @param communicationType
     *
     * @author Sumathi Gunasekaran
     * @Refactor Athira
     */

    boolean verifyCommunicationTypeInLogFile(Dut device, String communicationType, String logFileName) {
	LOGGER.debug("STARTING METHOD verifyCommunicationTypeInLogFile");
	String searchCommand = null;
	boolean status = false;
	searchCommand = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.GREP_COMMAND,
		communicationType, BroadBandTestConstants.SINGLE_SPACE_CHARACTER, logFileName);
	LOGGER.info("Search Command is: " + searchCommand);
	long startTime = System.currentTimeMillis();
	do {
	    status = CommonMethods.searchLogFiles(tapEnv, device, searchCommand);
	} while (!status && (System.currentTimeMillis() - startTime) < BroadBandTestConstants.EIGHT_MINUTE_IN_MILLIS
		&& BroadBandCommonUtils.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	if (!status) {
	    status = CommonUtils.validateTraceLog(tapEnv, device, communicationType,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, true);
	}
	LOGGER.debug("ENDING METHOD verifyCommunicationTypeInLogFile");
	return status;
    }

    /**
     * Method to verify Xconf Download is successful
     * 
     * @param tapEnv
     *            {@link AutomaticsTapApi} Reference
     * @param device
     *            Dut
     * @param communicationType
     *
     * @author Sumathi Gunasekaran
     * @Refactor Athira
     * 
     */
    boolean verifyDownloadIsSuccessful(Dut device) {
	LOGGER.debug("STARTING METHOD verifyDownloadIsSuccessful");
	boolean status = false;
	String cdlLogsForValidation = BroadBandXconfCdlUtils.getCdlLogsForValidation(tapEnv, device);
	String expectedLogMessage = FirmwareDownloadUtils.getCdlDownloadSuccessLog(device);
	status = CommonUtils.isGivenStringAvailableInCommandOutput(cdlLogsForValidation.toLowerCase(),
		expectedLogMessage.toLowerCase());
	if (!status) {
	    status = CommonUtils.validateTraceLog(tapEnv, device, expectedLogMessage,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, true);
	}
	LOGGER.debug("ENDING METHOD verifyDownloadIsSuccessful");
	return status;
    }

    /**
     * Access SSR and Xconf CDL via Direct Communication
     *
     * Test Case # 2: Verify Access SSR and Xconf CDL Via Direct Communication
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>STEP 1:Disable code big support</li>
     * <li>STEP 2:Enable parameter for uploading logs</li>
     * <li>STEP 3:verify log upload is through direct communication</li>
     * <li>STEP 4:verify SSR url points to codebig</li>
     * <li>STEP 5:Configure XCONF server with required configuration using REST API</li>
     * <li>STEP 6:Reboot the device</li>
     * <li>STEP 7:Verify XCONF communication is through Direct communication</li>
     * <li>STEP 8:Verify whether download request is accepted</li>
     * <li>STEP 9:Verify code download successful</li>
     * <li>STEP 10:Disable parameter for uploading logs</li>
     * <li>POST CONDITION:Downgrade the image to the previous image</li>
     * </ol>
     *
     * @author Sumathi Gunasekaran
     * @Refactor Athira
     * @param device
     *            {@link Dut}
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true, groups = {
	    BroadBandTestGroup.XCONF_CODE_DOWNLOAD })
    @TestDetails(testUID = "TC-RDKB-XCONF-CDL-1002")
    public void testAccessSSRAndXconfCDLViaDirectCommunication(Dut device) {
	// variable to store errorMessage
	String errorMessage = null;
	// variable to store testcaseId
	String testCaseId = "TC-RDKB-XCONF-CDL-002";
	// Variable to store step number
	String testStepNumber = "s1";
	// variable to store status
	boolean status = false;
	// variable to store current image
	String currentImageName = null;
	// variable to store expected log message
	String expectedLogMessage = null;
	// variable to store image name for cdl
	String imageNameForCdl = null;
	boolean hasLatestBuildChanged = false;
	boolean isCdlDataPosted = false;

	// download protocol
	String downloadProtocol = BroadBandTestConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP;
	// cdl log for validation
	String cdlLogsForValidation = null;

	String logFileName = null;

	try {

	    logFileName = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
		    BroadBandPropertyKeyConstants.LOGFILENAME_ATOM);

	} catch (Exception e) {
	    logFileName = BroadBandCommandConstants.FILE_CONSOLELOG;
	    LOGGER.info("Logfilename Set as ConsoleLog as no device specific value found");
	}

	try {
	    // STEP 1:Disable codebig support through dmcli command
	    errorMessage = "Failed to set value for code big support through dmcli command:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_CODEBIG_SUPPORT;
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION :Disable codebig support through webPA command");
	    LOGGER.info("STEP 1: ACTION :Execute WebpA command to disable codebig support");
	    LOGGER.info("STEP 1: Expected: The Dmcli command should set with default value false for codebig support");
	    status = DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_CODEBIG_SUPPORT,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_BOOLEAN_PARAMETER, BroadBandTestConstants.FALSE);
	    if (status) {
		LOGGER.info("STEP 1:ACTUAL:Successfully Disabled Code big support through parameter"
			+ BroadBandWebPaConstants.WEBPA_PARAM_CODEBIG_SUPPORT);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    // STEP 2 :Enable parameter for uploading logs
	    testStepNumber = "S2";
	    status = false;
	    errorMessage = "Failed to set value for parameter through dmcli command:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_UPLOAD_LOGS_NOW;
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 2 :DESCRIPTION: Enable parameter for uploading logs");
	    LOGGER.info("STEP 2: ACTION :Execute WebpA command to Enable parameter for uploading logs");
	    LOGGER.info("STEP 2 :Expected: The Dmcli command should set with value true for uploading logs parameter");
	    LOGGER.info("********************************************************");
	    status = DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_UPLOAD_LOGS_NOW,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_BOOLEAN_PARAMETER, BroadBandTestConstants.TRUE);
	    if (status) {
		LOGGER.info("STEP 2:ACTUAL:Successfully Enabled Upload Logs Now"
			+ BroadBandWebPaConstants.WEBPA_PARAM_UPLOAD_LOGS_NOW);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    // STEP 3 :verify log upload is through Direct Communication
	    testStepNumber = "S3";
	    status = false;
	    errorMessage = "Failed to verify log upload is through Direct communication";
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 3 :DESCRIPTION: verify log upload is through Direct Communication");
	    LOGGER.info(
		    "STEP 3: ACTION :Execute command to verify log upload should have communicated with Direct Communication");
	    LOGGER.info("STEP 3: Expected: The log upload should have communicated with Direct Communication");
	    LOGGER.info("********************************************************");
	    LOGGER.info("Waiting 5 Minutes for uploading logs");
	    tapEnv.waitTill(BroadBandTestConstants.FIVE_MINUTES);

	    status = verifyCommunicationTypeInLogFile(device, BroadBandTraceConstants.LOG_MESSAGE_DIRECT_COMMUNICATION,
		    logFileName);

	    if (status) {
		LOGGER.info("STEP 3:ACTUAL:Successfully verified log upload through Direct communication from log file"
			+ logFileName);
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    // STEP 4: verify SSR url points to direct communication url
	    testStepNumber = "S4";
	    status = false;
	    errorMessage = "Failed to verify SSR url points to Direct communication";
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 4:DESCRIPTION: verify SSR url points to direct communication");
	    LOGGER.info("STEP 4: ACTION :Execute command to verify SSR URL should point to the direct communication");
	    LOGGER.info("STEP 4:Expected: The SSR URL should point to the direct communication");
	    LOGGER.info("********************************************************");
	    String response = tapEnv.executeCommandUsingSsh(device,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
			    BroadBandTelemetryConstants.DCM_PROPERTIES_FILE_ETC_FOLDER,
			    BroadBandTestConstants.SYMBOL_PIPE, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.SINGLE_SPACE_CHARACTER,
			    BroadBandTestConstants.PROP_HTTP_UPLOAD_LINK_URL));

	    status = verifyCommunicationTypeInLogFile(device,
		    BroadBandTraceConstants.LOG_MESSAGE_DIRECT_COMMUNICATION_SERVER_URL
			    .replace(BroadBandTestConstants.STRING_REPLACE, CommonMethods.patternFinder(response,
				    BroadBandTraceConstants.LOG_PATTERN_FOR_HTTP_UPLOAD_LINK)),
		    logFileName);

	    if (status) {
		LOGGER.info("STEP 4:ACTUAL:Successfully verified SSR url points to Direct communication from log file"
			+ logFileName);
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    // STEP 5: Configure XCONF server with required configuration using
	    // REST API
	    testStepNumber = "S5";
	    status = false;
	    errorMessage = "Failed to configure XCONF server with required configuration using REST API";
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 5:DESCRIPTION: Configure XCONF server with required configuration using REST API");
	    LOGGER.info(
		    "STEP 5: ACTION :Execute command to configure the code download configuration using XCONF Rest API ");
	    LOGGER.info(
		    "STEP 5: Expected: configure the code download configuration using XCONF Rest API and swupdate.conf should updated with xconf url");
	    LOGGER.info("********************************************************");

	    try {
		currentImageName = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		LOGGER.info("CURRENT FIRMWARE VERSION: " + currentImageName);
		LOGGER.info("LATEST FIRMWARE VERSION: " + imageNameForCdl);
		if (CommonMethods.isNull(imageNameForCdl)) {
		    imageNameForCdl = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			    BroadBandPropertyKeyConstants.PARTIAL_PROPERTY_KEY_FOR_GA_IMAGE);
		    LOGGER.info("Latest Firmware version from property file: " + imageNameForCdl);
		}

		if (CommonMethods.isNotNull(imageNameForCdl)) {
		    if (!imageNameForCdl.contains(currentImageName)) {
			BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device, imageNameForCdl, false,
				downloadProtocol, BroadBandTestConstants.CONSTANT_5);
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
		LOGGER.info(
			"STEP 5:ACTUAL:Successfully configured XCONF server with required configuration using REST API");
	    } else {
		LOGGER.error("STEP 5: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    // STEP 6 :Reboot the device
	    testStepNumber = "S6";
	    status = false;
	    errorMessage = "Unable to connect RDKB device after successfull CDL";
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION:Reboot the device");
	    LOGGER.info("STEP 6: ACTION :Execute command to reboot the device");
	    LOGGER.info("STEP 6: Expected: STB should reboot and should comeup properly");
	    LOGGER.info("********************************************************");
	    status = CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device);
	    if (status) {
		LOGGER.info("STEP 6:ACTUAL:Successfully validated that given device comes up after successful CDL.");
	    } else {
		LOGGER.error("STEP 6: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    // STEP 7 :Verify XCONF communication is through direct
	    // communiation.
	    testStepNumber = "S7";
	    status = false;
	    errorMessage = "Unable to find the direct communication in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 7 :DESCRIPTION:Verify XCONF communication is through direct communication.");
	    LOGGER.info(
		    "STEP 7: ACTION :Execute command to verify communication should have happened through direct communication");
	    LOGGER.info("STEP 7 :Expected: The communication should have happened through direct communication");
	    LOGGER.info("********************************************************");
	    status = verifyCommunicationTypeInLogFile(device, BroadBandTraceConstants.LOG_MESSAGE_DIRECT_COMMUNICATION,
		    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    if (status) {
		LOGGER.info("STEP 7:ACTUAL: Successfully verified log upload through Direct communication");
	    } else {
		LOGGER.error("STEP 7: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);

	    // STEP 8 :Verify whether download request is accepted
	    testStepNumber = "S8";
	    status = false;
	    errorMessage = "Unable to find the XCONF HTTP CDL accepted message '" + expectedLogMessage + "' in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 8 :DESCRIPTION:Verify whether download request is accepted");
	    LOGGER.info("STEP 8: ACTION :Execute command to verify download request is accepted");
	    LOGGER.info(
		    "STEP 8: Expected: /rdklogs/logs/xconf.txt.0 file should contains the accepted message in the format of \""
			    + BroadBandCdlConstants.ACCEPTED_MESSAGE_FOR_XCONF_CODE_DOWNLOAD + "\"");
	    LOGGER.info("********************************************************");

	    LOGGER.info("STEP 8: imageNameForCdl : " + imageNameForCdl);
	    LOGGER.info("STEP 8: currentImageName : " + currentImageName);

	    cdlLogsForValidation = BroadBandXconfCdlUtils.getCdlLogsForValidation(tapEnv, device);
	    LOGGER.info("STEP 8: cdlLogsForValidation : " + cdlLogsForValidation);

	    try {
		// format current image name which is retrieved using webpa command to compare with current image
		// version
		currentImageName = BroadBandCommonUtils
			.removeDifferentSignedExtensionsInRequestedBuildName(currentImageName);
		LOGGER.info("STEP 8: currentImageName : " + currentImageName);

		imageNameForCdl = BroadBandCommonUtils
			.removeDifferentSignedExtensionsInRequestedBuildName(imageNameForCdl);
		LOGGER.info("STEP 8: imageNameForCdl : " + imageNameForCdl);

		// verify the rejection message
		expectedLogMessage = BroadBandCdlConstants.ACCEPTED_MESSAGE_FOR_XCONF_CODE_DOWNLOAD
			.replaceAll(BroadBandCdlConstants.CONFIGURATION_CURRENT_IMAGE_NAME, currentImageName)
			.replaceAll(BroadBandCdlConstants.CONFIGURATION_REQUESTED_IMAGE_NAME, imageNameForCdl);
		LOGGER.info("STEP 8: expectedLogMessage : " + expectedLogMessage);

		// workaround to overcome the type error in RDK Code
		cdlLogsForValidation = cdlLogsForValidation.replaceAll("imgae", "image");
		LOGGER.info("STEP 8: cdlLogsForValidation : " + cdlLogsForValidation);

		status = cdlLogsForValidation.toLowerCase().contains(expectedLogMessage.toLowerCase());
		// status = true;
		LOGGER.info("STEP 8: status : " + status);

		errorMessage = "Unable to find the XCONF " + downloadProtocol + " CDL accepted message '"
			+ expectedLogMessage + "' in " + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    } catch (Exception e) {
		errorMessage = e.getMessage();
		LOGGER.error(errorMessage);
	    }

	    // **********************************************************************************

	    if (status) {
		LOGGER.info("STEP 8:ACTUAL: Successfully validated the XCONF HTTP CDL accepted message in"
			+ BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0);
	    } else {
		LOGGER.error("STEP 8: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);

	    // STEP 9:Verify code download successful.
	    testStepNumber = "S9";
	    status = false;
	    errorMessage = null;
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION :Verify code download successful.");
	    LOGGER.info("STEP 9: ACTION :Execute command to verify code download successful");
	    LOGGER.info("STEP 9: Expected: " + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0
		    + " file should contains the code download success message");
	    LOGGER.info("********************************************************");
	    status = verifyDownloadIsSuccessful(device);
	    errorMessage = "Unable to retrieve Code download completed message from " + expectedLogMessage + " in "
		    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0;
	    LOGGER.info("STEP 9: ACTUAL: "
		    + (status
			    ? "Successfully validated the XCONF HTTP CDL completed message in "
				    + BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0
			    : errorMessage));
	    if (status) {
		hasLatestBuildChanged = status;
		LOGGER.info(
			"Wait for the device to reboot automatically , since we have set the set the reboot immedaitely to true");
		status = CommonMethods.isSTBRebooted(tapEnv, device, AutomaticsConstants.THIRTY_SECONDS,
			AutomaticsConstants.CONSTANT_10);
		LOGGER.info("STEP 9:status " + status);
		if (status) {
		    hasLatestBuildChanged = status;
		    LOGGER.info("Device rebooted after successful image download.Wait for the device to come up");
		    status = CommonMethods.waitForEstbIpAcquisition(tapEnv, device);

		    // wait for webpa to function properly
		    if (status) {
			status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
		    }
		    status = CodeDownloadUtils.verifyImageVersionFromVersionText(tapEnv, device, imageNameForCdl);
		    if (status) {
			LOGGER.info("Device successfully downloaded with image " + imageNameForCdl);
		    } else {
			errorMessage = "Image download to " + imageNameForCdl + " in device failed";
		    }
		} else {
		    errorMessage = "Device has not rebooted after Image download ";
		    LOGGER.error(errorMessage);
		}
	    } else {
		LOGGER.error(errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, true);
	    // STEP 10 :Enable parameter for uploading logs
	    testStepNumber = "S10";
	    status = false;
	    errorMessage = "Failed to set value for parameter through dmcli command:"
		    + BroadBandWebPaConstants.WEBPA_PARAM_UPLOAD_LOGS_NOW;
	    LOGGER.info("********************************************************");
	    LOGGER.info("STEP 10 :DESCRIPTION: Disable parameter for uploading logs");
	    LOGGER.info("STEP 10: ACTION :Execute command to disable parameter for uploading logs");
	    LOGGER.info(
		    "STEP 10 :Expected: The Dmcli command should set with value false for uploading logs parameter");
	    LOGGER.info("********************************************************");
	    status = DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_UPLOAD_LOGS_NOW,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_BOOLEAN_PARAMETER, BroadBandTestConstants.FALSE);
	    if (status) {
		LOGGER.info("STEP 10:ACTUAL: Successfully Enabled Upload Logs Now");
	    } else {
		LOGGER.error("STEP 10: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, testStepNumber, status, errorMessage, false);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying Direct communication.:" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, testStepNumber, status,
		    errorMessage, true);
	} finally {
	    int postCondNumber = BroadBandTestConstants.CONSTANT_0;

	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    tapEnv.executeCommandUsingSsh(device, "rm -rf /rdklogs/logs/xconf.txt");

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
	    // add signed extension to the image
	    String buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
	    try {
		buildExtension = BroadbandPropertyFileHandler.getAutomaticsPropsValueByResolvingPlatform(device,
			BroadBandPropertyKeyConstants.PROP_KEY_SIGNED_EXTENSION);
	    } catch (Exception e) {
		buildExtension = BroadBandTestConstants.SIGNED_BUILD_IMAGE_EXTENSION;
		LOGGER.info("No platform dpeendent extensions are mentioned in  automatics properties");

	    }
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
	    FirmwareDownloadUtils.deleteSoftwareUpdateConfigurationFile(tapEnv, device);
	}
    }

}
