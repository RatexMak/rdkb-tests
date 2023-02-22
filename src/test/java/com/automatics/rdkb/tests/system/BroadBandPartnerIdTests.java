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

import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.enums.ExecutionStatus;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandTraceConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.BroadBandXconfCdlUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandPartnerIdTests extends AutomaticsTestBase {
    /**
     * 
     * Test Case # 1: Verify the Partner Id parameter in any query to XCONF - CDL, DCM, and RFC.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1) Add Xconf url in swconf property file</li>
     * <li>PRE-CONDITION 2) Reboot the device
     * <li>S1) Verify partner id exist in DCM query URL from dcmscript.log file</li>
     * <li>S2) Verify partner id exist in RFC query URL from dcmrfc.log file</li>
     * <li>S3) Verify partner id exist in Xconf query URL from xconf.txt.0 file</li>
     * <li>POST-CONDITION 1. Remove Xconf url from swupdate conf file that is configured in precondition 1
     * </ol>
     *
     * @author BALAJI V
     *
     * @param device
     *            {@link Dut}
     * @refactor anandam
     * @refactor Govardhan
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-XCONF-PTNRID-5001")
    public void testPartnerIdParameterInXconfQuery(Dut device) {
	String testCaseId = "TC-RDKB-XCONF-PTNRID-501";
	boolean result = false;
	String step = "S1";
	String errorMessage = null;
	boolean status = false;
	boolean isTelemetryEnabled = BroadBandWebPaUtils.getAndVerifyWebpaValueInPolledDuration(device, tapEnv,
		BroadBandWebPaConstants.WEBPA_PARAM_FOR_TELEMETRY_2_0_ENABLE, BroadBandTestConstants.TRUE,
		BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS, BroadBandTestConstants.TEN_SECOND_IN_MILLIS);
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE:TC-RDKB-XCONF-PTNRID-5001");
	LOGGER.info("TEST DESCRIPTION: Verify Partner ID to all Xconf Query.");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1. Add Xconf url in swconf property file");
	LOGGER.info("PRE-CONDITION 2. Reboot the device");
	LOGGER.info("1. Verify partner id exist in DCM query URL from dcmscript.log file.");
	LOGGER.info("2. Verify partner id exist in RFC query URL from dcmrfc.log file");
	LOGGER.info("3. Verify partner id exist in Xconf query URL from xconf.txt.0 file");
	LOGGER.info("POST-CONDITION 1. Remove Xconf url from swupdate conf file that is configured in precondition 1");
	try {

	    String response = null;

	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    /**
	     * PRE-CONDITION 1: Add/Configure Xconf url in swconf property file
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 1 : DESCRIPTION : CONFIGURE /nvram/swupdate.conf FILE WITH MOCK XCONF URL");
	    LOGGER.info("PRE-CONDITION 1 : ACTION : EXECUTE echo <XCONF URL> > /nvram/swupdate.conf");
	    LOGGER.info("PRE-CONDITION 1 : EXPECTED : XCONF URL SHOULD BE CONFIGURED SUCCESSFULLY");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "FAILED TO CONFIGURE XCONF URL IN SWCONF PROPERTY FILE";
	    try {
		// Configure /nvram/swupdate.conf file with Mock Xconf url
		BroadBandXconfCdlUtils.updateSoftwareUpdateConfigurationOnClient(tapEnv, device);
		status = true;
		LOGGER.info("SUCCESSFULLY CONFIGURED /opt/swupdate.conf WITH MOCK XCONF URL");
	    } catch (Exception e) {
		errorMessage = "EXCEPTION WHILE CONFIGURING MOCK SERVER." + e.getMessage();
	    }
	    if (status) {
		LOGGER.info(
			"PRE-CONDITION 1 : ACTUAL : SUCCESSFULLY CONFIGURED /nvram/swupdate.conf WITH MOCK XCONF URL");
	    } else {
		LOGGER.error("PRE-CONDITION 1 : ACTUAL :" + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 1 : FAILED : " + errorMessage);
	    }
	    /**
	     * PRE-CONDITION 2: Perform reboot operation on the device
	     */
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("PRE-CONDITION 2 : DESCRIPTION  : REBOOT THE DEVICE");
	    LOGGER.info("PRE-CONDITION 2 : ACTION: EXECUTE reboot COMMAND");
	    LOGGER.info("PRE-CONDITION 2 : EXPECTED: THE BOX SHOULD REBOOT AND COME UP");
	    LOGGER.info("#######################################################################################");
	    errorMessage = "DEVICE IS NOT ACCESSIBLE AFTER REBOOT!!!";
	    status = BroadBandCommonUtils.rebootAndWaitForStbAccessible(device, tapEnv);

	    if (status) {
		LOGGER.info("PRE-CONDITION 2 : ACTUAL : SUCCESSFULLY REBOOTED DEVICE");
	    } else {
		LOGGER.error("PRE-CONDITION 2 : ACTUAL :" + errorMessage);
		throw new TestException(
			BroadBandTestConstants.PRE_CONDITION_ERROR + "PRE-CONDITION 2 : FAILED : " + errorMessage);
	    }
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * S1) Verify partner id exist in DCM query URL from dcmscript.log file
	     */
	    LOGGER.info(
		    "**************************************************************************************************");
	    LOGGER.info("STEP 1 : DESCRIPTION : VERIFY PARTNER ID EXIST IN DCM QUERY URL FROM DCMSCRIPT.LOG FILE.");
	    LOGGER.info(
		    "STEP 1 : ACTION : EXECUTE THE COMMAND : grep -i \"CURL_CMD:\\s*\" /rdklogs/logs/dcmscript.log.");
	    LOGGER.info("STEP 1 : EXPECTED :  DCM URL AND PARTNERID MUST BE PRESENT IN LOG MESSAGE.");
	    LOGGER.info(
		    "**************************************************************************************************");
	    errorMessage = "PARTNER ID NOT FOUND IN DCM/Telemetry 2.0 QUERY URL FROM dcmscript.log FILE";

	    if (!isTelemetryEnabled) {
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.TRACE_LOG_XCONF_CURL, BroadBandTestConstants.DCMSCRIPT_LOG_FILE,
			BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    } else {
		response = BroadBandCommonUtils.searchLogFiles(tapEnv, device,
			BroadBandTraceConstants.TRACE_LOG_TELEMETRY_2, BroadBandTestConstants.FILE_PATH_TELEMETRY_2_0,
			BroadBandTestConstants.FIFTEEN_MINUTES_IN_MILLIS,
			BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    }
	    result = CommonMethods.isNotNull(
		    CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_CURL_POST_QUERY_PARAM))
		    || CommonMethods.isNotNull(
			    CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_CURL_GET_QUERY_PARAM));

	    if (result) {
		LOGGER.info("STEP 1 : ACTUAL : PARTNER ID VERIFIED IN DCM/Telemetry 2.0 QUERY URL SUCCESSFULLY");
	    } else {
		LOGGER.error("STEP 1 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info(
		    "**************************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    /**
	     * S2) Verify partner id exist in RFC query URL from dcmrfc.log file
	     */
	    step = "S2";
	    LOGGER.info(
		    "**************************************************************************************************");
	    LOGGER.info("STEP 2 : DESCRIPTION : VERIFY PARTNER ID EXIST IN RFC QUERY URL FROM DCMRFC.LOG FILE.");
	    LOGGER.info("STEP 2 : ACTION : EXECUTE THE COMMAND : grep -i \"CURL_CMD:\\s*\" /rdklogs/logs/dcmrfc.log.");
	    LOGGER.info("STEP 2 : EXPECTED : RFC URL AND PARTNERID MUST BE PRESENT IN LOG MESSAGE.");
	    LOGGER.info(
		    "**************************************************************************************************");
	    result = false;
	    errorMessage = "PARTNER ID NOT FOUND IN DCM QUERY URL FROM dcmrfc.log FILE";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.TRACE_LOG_XCONF_CURL,
		    BroadBandTestConstants.DCMRFC_LOG_FILE, BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    result = CommonMethods.isNotNull(
		    CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_CURL_POST_QUERY_PARAM))
		    || CommonMethods.isNotNull(
			    CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_CURL_GET_QUERY_PARAM));
	    if (result) {
		LOGGER.info("STEP 2 : ACTUAL : PARTNER ID VERIFIED IN DCM QUERY URL SUCCESSFULLY");
	    } else {
		LOGGER.error("STEP 2 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info(
		    "**************************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, false);
	    /**
	     * S3) Verify partner id exist in Xconf query URL from xconf.txt.0 file
	     */
	    step = "S3";
	    LOGGER.info(
		    "**************************************************************************************************");
	    LOGGER.info("STEP 3 : DESCRIPTION : VERIFY PARTNER ID EXIST IN DCM QUERY URL FROM XCONF.TXT.0 FILE.");
	    LOGGER.info("STEP 3 : ACTION :  EXECUTE THE COMMAND : grep -i \"CURL_CMD:\\s*\" /rdklogs/logs/xconf.log.");
	    LOGGER.info("STEP 3 : EXPECTED :  XCONF URL AND PARTNERID MUST BE PRESENT IN LOG MESSAGE.");
	    LOGGER.info(
		    "**************************************************************************************************");
	    result = false;
	    errorMessage = "PARTNER ID NOT FOUND IN DCM QUERY URL FROM xconf.log FILE";
	    response = BroadBandCommonUtils.searchLogFiles(tapEnv, device, BroadBandTraceConstants.TRACE_LOG_XCONF_CURL,
		    BroadBandTestConstants.RDKLOGS_LOGS_XCONF_TXT_0, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    result = CommonMethods.isNotNull(
		    CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_CURL_POST_QUERY_PARAM))
		    || CommonMethods.isNotNull(
			    CommonMethods.patternFinder(response, BroadBandTestConstants.PATTERN_CURL_GET_QUERY_PARAM))
		    || CommonMethods.isNotNull(CommonMethods.patternFinder(response,
			    BroadBandTestConstants.PATTERN_CURL_GET_QUERY_PARAM_AFTER_RFC));
	    if (result) {
		LOGGER.info("STEP 3 : ACTUAL : PARTNER ID VERIFIED IN DCM QUERY URL SUCCESSFULLY");
	    } else {
		LOGGER.error("STEP 3 : ACTUAL : " + errorMessage);
	    }
	    LOGGER.info(
		    "**************************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, step, result, errorMessage, true);
	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING PARTNER ID TO ALL XCONF QUERY URL: " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, result, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("#######################################################################################");
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : REMOVE THE XCONF URL THAT IS CONFIGURED IN PRE-CONDITION 1");
	    LOGGER.info(
		    "POST-CONDITION 1 : ACTION : REMOVE THE XCONF URL THAT IS CONFIGURED IN PRE-CONDITION l FROM SWUPDATE CONF FILE");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : SUCCESSFULLY REMOVED URL CONFIGURED IN SWUPDATE CONF FILE");
	    LOGGER.info("#######################################################################################");
	    try {
		status = BroadBandXconfCdlUtils.toClearCdlInfoInXconf(device, tapEnv);
	    } catch (Exception e) {
		errorMessage = "EXCEPTION WHILE REMOVING XCONF URL" + e.getMessage();

	    }
	    if (status) {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : SUCCESSFULLY REMOVED XCONF URL FROM /nvram/swupdate.conf");
	    } else {
		LOGGER.error("POST-CONDITION 1 : ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");

	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-XCONF-PTNRID-5001");
    }

    /**
     * 
     *
     * Test Case # 1: Verify Partner ID using WebPA and TR-069.
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>S1) Verify PartnerID using webpa parameter</li>
     * <li>S2) Verify PartnerID using TR-69 parameter</li>
     * </ol>
     *
     * @author BALAJI V
     * @refactor Govardhan
     * 
     * @param device
     *            {@link Dut}
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-SYND-PTNRID-5002")
    public void testPartnerIdUsingWebPaAndTR069Params(Dut device) {
	String testCaseId = "TC-RDKB-SYND-PTNRID-502";
	boolean result = false;
	// Test step number
	String stepNum = null;
	// String to store error message
	String message = null;

	LOGGER.info("STARTING TEST CASE : TC-RDKB-SYND-PTNRID-5002");
	try {
	    // get the default partnerID
	    String defaultPartner = BroadbandPropertyFileHandler.getDefaultPartnerID();
	    if (CommonMethods.isNull(defaultPartner)) {
		throw new TestException("Default partner is not configured from external configuration");
	    }
	    // get the available syndication partner list
	    List<String> partnersList = BroadbandPropertyFileHandler.getPartnerListByResolvingPlatform(device);
	    /**
	     * S1) Verify PartnerID using webpa parameter
	     */
	    String webPaPartnerId = null;
	    stepNum = "s1";
	    LOGGER.info(
		    "**************************************************************************************************");
	    LOGGER.info("STEP 1 :DESCRIPTION : VERIFY PARTNERID USING WEBPA PARAMETER.");
	    LOGGER.info("STEP 1 : ACTION : EXECUTE WEBPA COMMAND TO VERIFY PARTNER ID.");
	    LOGGER.info("STEP 1 : EXPECTED :  WEBPA RESPONSE MUST RETURN THE PARTNER ID.");
	    LOGGER.info(
		    "**************************************************************************************************");
	    message = "Unable to verify the Syndicate Partner ID with WebPA Request.";
	    webPaPartnerId = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID);
	    LOGGER.info("PARTNER ID FROM WEBPA :" + webPaPartnerId);
	    result = CommonMethods.isNotNull(webPaPartnerId) && (webPaPartnerId.equalsIgnoreCase(defaultPartner)
		    || (!partnersList.isEmpty() && partnersList.contains(webPaPartnerId)));
	    LOGGER.info(
		    "**************************************************************************************************");
	    if (result) {
		LOGGER.info("STEP 1: ACTUAL: PARTNER ID VERIFIED SUCCESSFULLY USING WEBPA .");
	    } else {
		LOGGER.info("STEP 1: ACTUAL: " + message);
	    }
	    LOGGER.info(
		    "**************************************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, result, message, false);

	    /**
		 * S2) Verify PartnerID using TR-69 parameter
		 */
		stepNum = "s2";
		String tr69PartnerId = null;
		if (BroadbandPropertyFileHandler.isTr69Enabled()) {
			LOGGER.info("TR-69 is enabled...");
			LOGGER.info(
					"**************************************************************************************************");
			LOGGER.info("STEP 2: DESCRIPTION : VERIFY PARTNERID USING TR-069 PARAMETER.");
			LOGGER.info("STEP 2: ACTION : EXECUTE TR-069 COMMAND TO VERIFY PARTNERID");
			LOGGER.info("STEP 2: EXPECTED : TR-069 RESPONSE MUST RETURN THE PARTNER ID.");
			LOGGER.info(
					"**************************************************************************************************");
			result = false;
			message = "Unable to verify the Syndicate Partner ID with TR-69 Request.";
			tr69PartnerId = tapEnv.executeTr69CommandWithTimeOut(device,
					BroadBandWebPaConstants.WEBPA_PARAM_FOR_SYNDICATION_PARTNER_ID,
					BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
			LOGGER.info("PARTNER ID FROM TR-069:" + tr69PartnerId);
			result = CommonMethods.isNotNull(tr69PartnerId) && (tr69PartnerId.equalsIgnoreCase(defaultPartner)
					|| (!partnersList.isEmpty() && partnersList.contains(tr69PartnerId)));
			LOGGER.info(
					"**************************************************************************************************");

			if (result) {
				LOGGER.info("STEP 2: ACTUAL: PARTNER ID VERIFIED SUCCESSFULLY USING TR-069.");
			} else {
				LOGGER.info("STEP 2: ACTUAL: " + message);
			}
			LOGGER.info(
					"**************************************************************************************************");
			tapEnv.updateExecutionStatus(device, testCaseId, stepNum, result, message, false);
		} else {
			message = "TR-69 is disabled so skipping teststep ...";
			LOGGER.error(message);
			tapEnv.updateExecutionForAllStatus(device, testCaseId, stepNum, ExecutionStatus.NOT_APPLICABLE, message,
					false);
		}
	} catch (Exception exception) {
		message = exception.getMessage();
		LOGGER.error("EXCEPTION OCCURRED WHILE VALIDATING PARTNER ID USING WEBPA AND TR-069: " + message);
		CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, result, message, true);
	}
	LOGGER.info("ENDING TEST CASE : TC-RDKB-SYND-PTNRID-5002");
    }

}
