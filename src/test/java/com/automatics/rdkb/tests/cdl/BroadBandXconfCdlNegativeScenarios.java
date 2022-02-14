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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.BroadBandCommonUtils;

/**
 * Class to hold all the test scripts for XCONF CDL negative scenarios
 * 
 * @author anandam.s
 * 
 */
public class BroadBandXconfCdlNegativeScenarios  extends AutomaticsTestBase{
    /**
     * Verify the Xconf flag support the delay download at installation with invalid images
     * <ol>
     * <li>Configure the mock xconf server for CDL with delayDownload 30 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * <li>Configure the mock xconf server for CDL with delayDownload 60 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * <li>Configure the mock xconf server for CDL with delayDownload -3 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * <li>Configure the mock xconf server for CDL with delayDownload 0 mins</li>
     * <li>Trigger the CDL through webpa</li>
     * <li>verify the delay download log is logged in xconf.txt.0</li>
     * </ol>
     * 
     * @param device
     * @author prasanthreddy.a
	 * @Refactor Athira
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = BroadBandTestGroup.SYSTEM)
    @TestDetails(testUID = "TC-RDKB-XCONF-DELAY-1001")
    public void testToValidateXconfDelay(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-XCONF-DELAY-101";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-XCONF-DELAY-1001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the Xconf flag support the delay download at installation with invalid images");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Configure the mock xconf server for CDL with delayDownload 30 mins");
	LOGGER.info("2. Trigger the CDL through webpa");
	LOGGER.info("3. verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("4. Configure the mock xconf server for CDL with delayDownload 60 mins");
	LOGGER.info("5. Trigger the CDL through webpa");
	LOGGER.info("6. verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("7. Configure the mock xconf server for CDL with delayDownload -3 mins");
	LOGGER.info("8. Trigger the CDL through webpa");
	LOGGER.info("9. verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("10. Configure the mock xconf server for CDL with delayDownload 0 mins");
	LOGGER.info("11. Trigger the CDL through webpa");
	LOGGER.info("12. verify the delay download log is logged in xconf.txt.0");

	LOGGER.info("#######################################################################################");

	try {
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_1,
		    BroadBandTestConstants.CONSTANT_30);
	    // Step 4 to Step 6 Validate delay download with 60 Mins
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_4,
		    BroadBandTestConstants.CONSTANT_60);
	    // Step 7 to Step 9 Validate delay download with -3 Mins
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_7,
		    BroadBandTestConstants.CONSTANT_NEGATIVE_3);
	    // Step 10 to Step 12 Validate delay download with 0 Mins
	    helperMethodtoValidateXconfDelayDownload(device, tapEnv, testCaseId, BroadBandTestConstants.CONSTANT_10,
		    BroadBandTestConstants.CONSTANT_0);
	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-DELAY-1001");
    }
    
	 /**
     * Helper Method to validate delay download validations
     * 
     * @param device
     *            Dut instance
     * @param tapEnv
     *            AutomaticsTapApi instance
     * @param testCaseId
     *            String testcaseid
     * @param stepNumber
     *            int stepnumber
     * @param delayDownload
     *            int delaydownload
     */
    public void helperMethodtoValidateXconfDelayDownload(Dut device, AutomaticsTapApi tapEnv, String testCaseId,
    	    int stepNumber, int delayDownload) {
    	// Variable Declaration begins

    	String stepNum = "";
    	String errorMessage = "";
    	boolean status = false;
    	// download protocol
    	String downloadProtocol = AutomaticsConstants.FIRMWARE_DOWNLOAD_PROTOCOL_HTTP;
    	// Variable Declaration Ends
    	String response = null;
    	stepNum = "s" + stepNumber;
    	errorMessage = "failed to configure the mock xconf details";
    	status = false;
    	LOGGER.info("**********************************************************************************");
    	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Configure the mock xconf server for CDL with delayDownload"
    		+ delayDownload + " mins");
    	LOGGER.info("STEP " + stepNumber
    		+ ": ACTION : Configure mock xconf similar to CDL process) Update /nvram/swupdate.conf file with mock xconf url.echo \"https://<url>/xconf/swu/stb\" > /nvram/swupdate.conf2) Configure the different image name of the device in the mock xconf server3)Add delay as 30 minutes and reboot immediately as false");
    	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Mock xconf configuration should be successful");
    	LOGGER.info("**********************************************************************************");
    
	try {

	    BroadBandXconfCdlUtils.configureRdkbDeviceForXconfCdl(tapEnv, device,
		    BroadBandTestConstants.TEMPORARY_FOLDER, false, downloadProtocol, delayDownload);
	    status = true;
	} catch (Exception e) {
	    errorMessage = "Exception occured while configuring xconf server " + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber
		    + ": ACTUAL: Successfully Configured XCONF server with required configuration.");
	} else {
	    LOGGER.info("STEP " + stepNumber + " : ACTUAL :" + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Unable to execute webpa command";
	status = false;

	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : Trigger the CDL through webpa");
	LOGGER.info("STEP " + stepNumber
		+ ": ACTION : Execute command to set value true to Webpa param for trigger CDL");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Webpa should be successful");
	LOGGER.info("**********************************************************************************");
	
	try {
	    status = BroadBandXconfCdlUtils.initiateXconfCdlThroughWebpa(tapEnv, device);
	} catch (Exception e) {
	    errorMessage = "Exception occured while initiating cdl throgh webpa " + e.getMessage();
	    LOGGER.error(errorMessage);
	}
	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL: Successfully initiated cdl using webpa command.");
	} else {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL :" + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);
	
	stepNumber++;
	stepNum = "s" + stepNumber;
	errorMessage = "Failed to verify the delay download log message";
	status = false;
	response = null;
	LOGGER.info("**********************************************************************************");
	LOGGER.info("STEP " + stepNumber + ": DESCRIPTION : verify the delay download log is logged in xconf.txt.0");
	LOGGER.info("STEP " + stepNumber + ": ACTION : grep -I \"Device configured with download delay of "
		+ delayDownload + " minutes\" or Resetting the download delay to 0 minutes /rdklogs/logs/xconf.txt.0");
	LOGGER.info("STEP " + stepNumber + ": EXPECTED : Device configured with download delay of " + delayDownload
		+ " minutes log should be present in xconf.txt.0");
	LOGGER.info("**********************************************************************************");
	delayDownload = delayDownload <= 0 ? BroadBandTestConstants.CONSTANT_0 : delayDownload;
	String validateText = delayDownload == 0 ? BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_RESET
		: BroadBandTestConstants.STRING_FOR_DELAY_DOWNLOAD_SPECIFIED_TIME
			.replace(BroadBandTestConstants.STRING_REPLACE, String.valueOf(delayDownload));
	try {
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, validateText,
		    BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH,
		    BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, validateText);
	} catch (Exception e) {
	    LOGGER.error("Exception occured while executing command:" + e.getMessage());
	}
	if (status && delayDownload == 0) {
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTestConstants.LOG_CDL_STARTED,
		    BroadBandCommandConstants.STRING_XCONF_BACKUP_FILEPATH, BroadBandTestConstants.ONE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    status = CommonMethods.isNotNull(response)
		    && CommonUtils.patternSearchFromTargetString(response, BroadBandTestConstants.LOG_CDL_STARTED);

	}

	if (status) {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : Delay download log validated : " + validateText);
	} else {
	    LOGGER.info("STEP " + stepNumber + ": ACTUAL : " + errorMessage);
	}
	LOGGER.info("**********************************************************************************");
	tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	
    }
    
}
