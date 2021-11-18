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
package com.automatics.rdkb.tests.wifi;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.TestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Test class with test case related to Radio status validation
 * 
 * @author Praveenkumar Paneerselvam
 * @refactor Govardhan
 */
public class BroadbandRadioStatusWifiTest extends AutomaticsTestBase{

    /**
     * <li>1. reboot the device & verify webpa process is up</li>
     * <li>2. Update the telemetry log upload interval to 5 min</li>
     * <li>3. Get operating channel bandwidth for 2.4GHz using webpa</li>
     * <li>4. Get operating channel bandwidth for 5GHz using webpa</li>
     * <li>5. Verify operating channel bandwidth for 2.4GHz using wifihealth log</li>
     * <li>6. Verify operating channel bandwidth for 5GHz using wifihealth log</li>
     * <li>7. Update the channel bandwidth for 2.4GHz using webpa</li>
     * <li>8. Update the channel bandwidth for 5GHz using webpa</li>
     * <li>9. Verify operating channel bandwidth for 2.4GHz using wifihealth log</li>
     * <li>10. Verify operating channel bandwidth for 5GHz using wifihealth log</li>
     * <li>11. Revert the channel bandwidth for 2.4GHz and 5GHz using webpa</li>
     * 
     * @author Praveenkumar Paneerselvam
     * @refactor Govardhan
     */

    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    TestGroup.NEW_FEATURE, TestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-1016")
    public void testToVerifyChannelBandWidth(Dut device) {
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-1016");
	LOGGER.info("TEST DESCRIPTION: Test to verify wifi channel bandwidth");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. reboot the device & verify webpa process is up");
	LOGGER.info("2. Update the telemetry log upload interval to 5 min");
	LOGGER.info("3. Get operating channel bandwidth for 2.4GHz using webpa");
	LOGGER.info("4. Get operating channel bandwidth for 5GHz using webpa");
	LOGGER.info("5. Verify operating channel bandwidth for 2.4GHz using wifihealth log");
	LOGGER.info("6. Verify operating channel bandwidth for 5GHz using wifihealth log");
	LOGGER.info("7. Update the channel bandwidth for 2.4GHz using webpa");
	LOGGER.info("8. Update the channel bandwidth for 5GHz using webpa");
	LOGGER.info("9. Verify operating channel bandwidth for 2.4GHz using wifihealth log");
	LOGGER.info("10. Verify operating channel bandwidth for 5GHz using wifihealth log");
	LOGGER.info("11. Revert the channel bandwidth for 2.4GHz and 5GHz using webpa");
	LOGGER.info("#######################################################################################");

	// variable declaration begins
	// Status of test script verification
	boolean status = false;
	// Test case id
	String testCaseId = "TC-RDKB-WIFI-016";
	// Test step number
	String stepNumber = "s1";
	// String to store error message
	String errorMessage = null;
	// String to store response
	String response = null;
	// String to store the command
	String command = null;
	// String to store 2.4GHz channel bandwidth
	String channelBw2Ghz = null;
	// String to store 5GHz channel bandwidth
	String channelBw5Ghz = null;
	// String to store 2.4GHz updated channel bandwidth
	String updatedChannelBandWid2Ghz = null;
	// String to store 5GHz updated channel bandwidth
	String updatedChannelBandWid5Ghz = null;
	// variable declaration ends

	try {

	    stepNumber = "s1";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION: reboot the device & verify webpa process is up");
	    LOGGER.info("STEP 1: ACTION: Execute command: /sbin/reboot");
	    LOGGER.info("STEP 1: EXPECTED: Device should be SSHable after reboot & webpa process should be up");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to reboot the device";
	    if (CommonUtils.rebootAndWaitForIpAcquisition(tapEnv, device)) {
		errorMessage = "Failed to verify webpa process up after reboot";
		status = BroadBandWebPaUtils.verifyWebPaProcessIsUp(tapEnv, device, true);
	    }
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: Successfully rebooted the device & webpa process is up");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber = "s2";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION: Update the telemetry log upload interval to 5 min");
	    LOGGER.info("STEP 2: ACTION: Execute command: 1.echo 5 > /tmp/upload 2.ls /tmp/upload");
	    LOGGER.info("STEP 2: EXPECTED: upload file should contain the value as 5");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update upload interval value in upload file";
	    CommonMethods.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommandConstants.CMD_UPLOAD_TIME, CommonMethods.isAtomSyncAvailable(device, tapEnv));
	    response = CommonMethods.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandTestConstants.CAT_COMMAND,
			    BroadBandCommandConstants.FILE_UPLOAD),
		    CommonMethods.isAtomSyncAvailable(device, tapEnv)).trim();
	    status = CommonMethods.isNotNull(response) && response.equalsIgnoreCase(BroadBandTestConstants.STRING_5);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL: Successfully updated the upload file with interval time as 5");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s3";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION: Get operating channel bandwidth for 2.4GHz using webpa");
	    LOGGER.info("STEP 3: ACTION: Execute webpa command: Device.WiFi.Radio.10000.OperatingChannelBandwidth");
	    LOGGER.info("STEP 3: EXPECTED: Response should contain the channel bandwidth for 2.4GHz");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for webpa parameter Device.WiFi.Radio.10000.OperatingChannelBandwidth";
	    channelBw2Ghz = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND);
	    status = CommonMethods.isNotNull(channelBw2Ghz);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Successfully got 2.4GHz operating channel bandwidth as: " + channelBw2Ghz);
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber = "s4";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION: Get operating channel bandwidth for 5GHz using webpa");
	    LOGGER.info("STEP 4: ACTION: Execute webpa command: Device.WiFi.Radio.10100.OperatingChannelBandwidth");
	    LOGGER.info("STEP 4: EXPECTED: Response should contain the channel bandwidth for 5GHz");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the response for webpa parameter Device.WiFi.Radio.10100.OperatingChannelBandwidth";
	    channelBw5Ghz = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_5GHZ_BAND);
	    status = CommonMethods.isNotNull(channelBw5Ghz);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL: Successfully got 5GHz operating channel bandwidth as: " + channelBw5Ghz);
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, true);
	    // ##################################################################################################//

	    stepNumber = "s5";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION: Verify operating channel bandwidth for 2.4GHz using wifihealth log");
	    LOGGER.info(
		    "STEP 5: ACTION: Execute command: grep -i WiFi_config_2G_chan_width_split /rdklogs/logs/wifihealth.txt");
	    LOGGER.info("STEP 5: EXPECTED: Response should contain the log message");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message WiFi_config_2G_chan_width_split in /rdklogs/logs/wifihealth.txt";
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_WIFI_2G_CHAN_WIDTH,
		    BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_WIFI_2G_CHAN_WIDTH);
		LOGGER.info("STEP 5: wifi channel bandwidth in wifihealth.txt file is: " + response);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(channelBw2Ghz, response);
	    }
	    if (status) {
		LOGGER.info("STEP 5: ACTUAL: Successfully verified wifi channel bandwidth for 2.4GHz");
	    } else {
		LOGGER.error("STEP 5: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s6";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION: Verify operating channel bandwidth for 5GHz using wifihealth log");
	    LOGGER.info(
		    "STEP 6: ACTION: Execute command: grep -I WiFi_config_5G_chan_width_split /rdklogs/logs/wifihealth.txt");
	    LOGGER.info("STEP 6: EXPECTED: Response should contain the log message");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message WiFi_config_5G_chan_width_split in /rdklogs/logs/wifihealth.txt";
	    response = BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device, tapEnv,
		    BroadBandTraceConstants.LOG_MESSAGE_WIFI_5G_CHAN_WIDTH,
		    BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    if (CommonMethods.isNotNull(response)) {
		response = CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_WIFI_5G_CHAN_WIDTH);
		LOGGER.info("STEP 6: wifi channel bandwidth in wifihealth.txt file is: " + response);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(channelBw5Ghz, response);
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL: Successfully verified wifi channel bandwidth for 5GHz");
	    } else {
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s7";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION: Update the channel bandwidth for 2.4GHz using webpa");
	    LOGGER.info("STEP 7: ACTION: Execute webpa set command: Device.WiFi.Radio.10000.OperatingChannelBandwidth");
	    LOGGER.info("STEP 7: EXPECTED: Webpa set operation should success");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update the channel bandwidth for 2.4GHz using webpa";
	    updatedChannelBandWid2Ghz = channelBw2Ghz.equalsIgnoreCase(BroadBandTestConstants.CHANNEL_WIDTH_40MHZ)
		    ? BroadBandTestConstants.CHANNEL_WIDTH_20MHZ : BroadBandTestConstants.CHANNEL_WIDTH_40MHZ;
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND,
		    WebPaDataTypes.STRING.getValue(), updatedChannelBandWid2Ghz,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL: Successfully updated the wifi channel bandwidth for 2.4GHz as " + command);
	    } else {
		LOGGER.error("STEP 7: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s8";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 8: DESCRIPTION: Update the channel bandwidth for 5GHz using webpa");
	    LOGGER.info("STEP 8: ACTION: Execute webpa set command: Device.WiFi.Radio.10100.OperatingChannelBandwidth");
	    LOGGER.info("STEP 8: EXPECTED: Webpa set operation should success");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to update the channel bandwidth for 5GHz using webpa";
	    updatedChannelBandWid5Ghz = channelBw5Ghz.equalsIgnoreCase(BroadBandTestConstants.CHANNEL_WIDTH_80MHZ)
			? BroadBandTestConstants.CHANNEL_WIDTH_40MHZ
			: BroadBandTestConstants.CHANNEL_WIDTH_80MHZ;
	    
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_5GHZ_BAND,
		    WebPaDataTypes.STRING.getValue(), updatedChannelBandWid5Ghz,
		    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL: Successfully updated the wifi channel bandwidth for 5GHz as " + command);
	    } else {
		LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s9";
	    status = false;
	    response = CommonMethods.patternFinder(updatedChannelBandWid2Ghz,
		    BroadBandTestConstants.PATTERN_MATCHER_GET_NUMBER);
	    if (CommonMethods.isNotNull(response)) {
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommonUtils
			.concatStringUsingStringBuffer(BroadBandTraceConstants.LOG_MESSAGE_WIFI_2G_CHAN_WIDTH,
				BroadBandTestConstants.CHARACTER_COLON.toString()),
			response);
	    }
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION: Verify operating channel bandwidth for 2.4GHz using wifihealth log");
	    LOGGER.info("STEP 9: ACTION: Execute command: grep -i " + command + " /rdklogs/logs/wifihealth.txt");
	    LOGGER.info("STEP 9: EXPECTED: Response should contain the log message");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message WiFi_config_2G_chan_width_split in wifihealth.txt file";
	    if (CommonMethods.isNotNull(command)) {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
			tapEnv, command, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info(
			"STEP 9: ACTUAL: Successfully verified updated wifi channel bandwidth for 2.4GHz in wifihealth log");
	    } else {
		LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s10";
	    status = false;
	    response = CommonMethods.patternFinder(updatedChannelBandWid5Ghz,
		    BroadBandTestConstants.PATTERN_MATCHER_GET_NUMBER);
	    if (CommonMethods.isNotNull(response)) {
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommonUtils
			.concatStringUsingStringBuffer(BroadBandTraceConstants.LOG_MESSAGE_WIFI_5G_CHAN_WIDTH,
				BroadBandTestConstants.CHARACTER_COLON.toString()),
			response);
	    }
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION: Verify operating channel bandwidth for 5GHz using wifihealth log");
	    LOGGER.info("STEP 10: ACTION: Execute command: grep -i " + command + " /rdklogs/logs/wifihealth.txt");
	    LOGGER.info("STEP 10: EXPECTED: Response should contain the log message");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to get the log message WiFi_config_5G_chan_width_split in wifihealth.txt file";
	    if (CommonMethods.isNotNull(command)) {
		status = CommonMethods.isNotNull(BroadBandCommonUtils.searchLogFilesInAtomOrArmConsoleByPolling(device,
			tapEnv, command, BroadBandCommandConstants.LOCATION_WIFI_HEALTH_LOG,
			BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));
	    }
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL: Successfully verified updated wifi channel bandwidth for 5GHz in wifihealth log");
	    } else {
		LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	    stepNumber = "s11";
	    status = false;
	    LOGGER.info("******************************************************************************");
	    LOGGER.info("STEP 11: DESCRIPTION: Revert the channel bandwidth for 2.4GHz and 5GHz using webpa");
	    LOGGER.info(
		    "STEP 11: ACTION: Execute webpa set command: Device.WiFi.Radio.10000.OperatingChannelBandwidth & Device.WiFi.Radio.10100.OperatingChannelBandwidth");
	    LOGGER.info("STEP 11: EXPECTED: Should be reverted channel bandwidth for 2.4GHz & 5GHz");
	    LOGGER.info("******************************************************************************");
	    errorMessage = "Failed to revert the channel bandwidth for 2.4GHz and 5GHz value using webpa";
	    status = BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_2GHZ_BAND,
		    WebPaDataTypes.STRING.getValue(), channelBw2Ghz, BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS)
		    && BroadBandWebPaUtils.setVerifyWebPAInPolledDuration(device, tapEnv,
			    BroadBandWebPaConstants.WEBPA_PARAM_FOR_OPERATING_BANDWIDTH_IN_5GHZ_BAND,
			    WebPaDataTypes.STRING.getValue(), channelBw5Ghz,
			    BroadBandTestConstants.THREE_MINUTE_IN_MILLIS,
			    BroadBandTestConstants.FIFTY_SECONDS_IN_MILLIS);
	    if (status) {
		LOGGER.info("STEP 11: ACTUAL: Successfully reverted the Wifi channel bandwidth for both 2.4GHz & 5GHz");
	    } else {
		LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
	    }
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNumber, status, errorMessage, false);
	    // ##################################################################################################//

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception Occurred while Verifying wifi channel bandwidth for 2.4GHz & 5GHz" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNumber, status, errorMessage,
		    false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    LOGGER.info("POST-CONDITION: DESCRIPTION: Remove the upload file from the device");
	    LOGGER.info("POST-CONDITION: ACTION: Execute command: rm /tmp/upload");
	    LOGGER.info("POST-CONDITION: EXPECTED:  upload file should be removed from the device");
	    CommonMethods.executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
		    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_RM_WITH_R_F_OPTION,
			    BroadBandCommandConstants.FILE_UPLOAD),
		    CommonMethods.isAtomSyncAvailable(device, tapEnv));
	    response = CommonMethods
		    .executeCommandInAtomConsoleIfAtomIsPresentElseInArm(device, tapEnv,
			    BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
				    BroadBandCommandConstants.FILE_UPLOAD),
			    CommonMethods.isAtomSyncAvailable(device, tapEnv));
	    status = CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandTestConstants.NO_SUCH_FILE_OR_DIRECTORY);
	    if (status) {
		LOGGER.info("POST-CONDITION: ACTUAL: Successfully removed upload file from /tmp directory");
	    } else {
		LOGGER.error("POST-CONDITION: ACTUAL: Failed to remove the upload file from /tmp directory");
	    }
	    LOGGER.info("POST-CONFIGURATIONS: FINAL STATUS - " + status);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-1016");
	// ###############################################################//
    }
}
